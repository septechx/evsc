use std::convert::TryInto;

use anyhow::{Result, bail};
use thin_vec::ThinVec;

use crate::{
    ast::{
        Block, Expr, ExprKind, Ident, Literal,
        expressions::{
            ArrayLiteralExpr, AsExpr, AssignmentExpr, BinaryExpr, BlockExpr, FunctionCallExpr,
            MemberAccessExpr, PostfixExpr, PrefixExpr, StructInstantiationExpr, SymbolExpr,
            TupleLiteralExpr, TypeExpr,
        },
    },
    fatal_at,
    hashmap::FxHashMap,
    lexer::token::TokenKind,
    parser::{
        Parser,
        lookups::{BP_LU, BindingPower, LED_LU, NUD_LU},
        stmt::parse_stmt,
        string::process_string,
        types::parse_type,
        utils::unexpected_token,
    },
    span::Span,
};

pub fn parse_expr(parser: &mut Parser, bp: BindingPower) -> Result<Expr> {
    let token = parser.current_token();
    let start_span = token.span;

    let bp_lu = BP_LU.get().expect("Lookups not initialized");
    let nud_lu = NUD_LU.get().expect("Lookups not initialized");
    let led_lu = LED_LU.get().expect("Lookups not initialized");

    let nud_fn = nud_lu
        .get(&token.kind)
        .cloned()
        .unwrap_or_else(|| unexpected_token(token.clone()));

    let mut left = nud_fn(parser)?;

    let mut end_span = left.span;

    loop {
        let current_bp = *bp_lu
            .get(&parser.current_token().kind)
            .unwrap_or(&BindingPower::DefaultBp);

        if current_bp <= bp {
            break;
        }

        let token_kind = parser.current_token();
        let led_fn = led_lu
            .get(&token_kind.kind)
            .cloned()
            .unwrap_or_else(|| unexpected_token(token_kind.clone()));

        left = led_fn(parser, left.clone(), current_bp)?;
        end_span = Span::new(start_span.start(), parser.current_token().span.end());
    }

    left.span = Span::new(start_span.start(), end_span.end());
    Ok(left)
}

pub fn parse_primary_expr(parser: &mut Parser) -> Result<Expr> {
    let value = parser.current_token().value.clone();
    let token = parser.advance();
    let span = token.span;

    match token.kind {
        TokenKind::Number => {
            if value.contains('.') {
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Float(value.parse::<f64>()?)),
                    span,
                })
            } else {
                Ok(Expr {
                    kind: ExprKind::Literal(Literal::Integer(value.parse::<i64>()?)),
                    span,
                })
            }
        }
        TokenKind::StringLiteral => Ok(Expr {
            kind: ExprKind::Literal(Literal::String(
                process_string(&value, span, token.module_id).into_boxed_str(),
            )),
            span,
        }),
        TokenKind::CharLiteral => Ok(Expr {
            kind: ExprKind::Literal(Literal::Char(
                value.chars().next().expect("value has a char"),
            )),
            span,
        }),
        TokenKind::Identifier => Ok(Expr {
            kind: ExprKind::Symbol(SymbolExpr {
                value: TryInto::<Ident>::try_into(token)?,
            }),
            span,
        }),
        TokenKind::True => Ok(Expr {
            kind: ExprKind::Literal(Literal::Bool(true)),
            span,
        }),
        TokenKind::False => Ok(Expr {
            kind: ExprKind::Literal(Literal::Bool(false)),
            span,
        }),
        _ => unreachable!(),
    }
}

pub fn parse_binary_expr(parser: &mut Parser, left: Expr, bp: BindingPower) -> Result<Expr> {
    let operator = parser.advance();
    let right = parse_expr(parser, bp)?;

    let span = Span::new(left.span.start(), right.span.end());
    Ok(Expr {
        kind: ExprKind::Binary(BinaryExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }),
        span,
    })
}

pub fn parse_postfix_expr(parser: &mut Parser, left: Expr, _bp: BindingPower) -> Result<Expr> {
    let nud_lu = NUD_LU.get().expect("Lookups not initialized");

    if parser.current_token().kind == TokenKind::Star && nud_lu.contains_key(&parser.peek().kind) {
        return parse_binary_expr(parser, left, BindingPower::Multiplicative);
    }

    let operator = parser.advance();

    let span = Span::new(left.span.start(), operator.span.end());
    Ok(Expr {
        kind: ExprKind::Postfix(PostfixExpr {
            left: Box::new(left),
            operator,
        }),
        span,
    })
}

pub fn parse_prefix_expr(parser: &mut Parser) -> Result<Expr> {
    let operator = parser.advance();
    let right = parse_expr(parser, BindingPower::DefaultBp)?;

    let span = Span::new(operator.span.start(), right.span.end());
    Ok(Expr {
        kind: ExprKind::Prefix(PrefixExpr {
            operator,
            right: Box::new(right),
        }),
        span,
    })
}

pub fn parse_assignment_expr(
    parser: &mut Parser,
    assigne: Expr,
    _bp: BindingPower,
) -> Result<Expr> {
    let operator = parser.advance();
    let value = parse_expr(parser, BindingPower::Assignment)?;

    let span = Span::new(assigne.span.start(), value.span.end());
    Ok(Expr {
        kind: ExprKind::Assignment(AssignmentExpr {
            assigne: Box::new(assigne),
            operator,
            value: Box::new(value),
        }),
        span,
    })
}

pub fn parse_struct_instantiation_expr(
    parser: &mut Parser,
    left: Expr,
    _bp: BindingPower,
) -> Result<Expr> {
    let struct_name = match &left.kind {
        ExprKind::Symbol(sym) => sym.value.clone(),
        _ => bail!("Expected symbol for struct instantiation"),
    };

    parser.expect(TokenKind::OpenCurly)?;

    let mut properties: FxHashMap<Ident, Expr> = FxHashMap::default();

    loop {
        if parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        let property = parser.expect_identifier()?;

        let value = if parser.current_token().kind == TokenKind::Colon {
            parser.expect(TokenKind::Colon)?;
            parse_expr(parser, BindingPower::Assignment)?
        } else {
            Expr {
                kind: ExprKind::Symbol(SymbolExpr {
                    value: property.clone(),
                }),
                span: property.span,
            }
        };

        properties.insert(property, value);

        if parser.current_token().kind != TokenKind::CloseCurly {
            parser.expect(TokenKind::Comma)?;
        }
    }

    let close_token = parser.expect(TokenKind::CloseCurly)?;

    let span = Span::new(left.span.start(), close_token.span.end());
    Ok(Expr {
        kind: ExprKind::StructInstantiation(StructInstantiationExpr {
            name: struct_name,
            fields: properties,
        }),
        span,
    })
}

// TODO: This only parses slices, not arrays
// Maybe fine, as this is just an array literal and can be treated as both and have the correct type inferred
pub fn parse_array_literal_expr(parser: &mut Parser) -> Result<Expr> {
    let start_token = parser.expect(TokenKind::OpenBracket)?;
    parser.expect(TokenKind::CloseBracket)?;

    let type_ = parse_type(parser, BindingPower::DefaultBp)?;

    parser.expect(TokenKind::OpenCurly)?;

    let mut contents: ThinVec<Expr> = ThinVec::new();

    loop {
        if parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        contents.push(parse_expr(parser, BindingPower::Assignment)?);

        if parser.current_token().kind != TokenKind::CloseCurly {
            parser.expect(TokenKind::Comma)?;
        }
    }

    let close_token = parser.expect(TokenKind::CloseCurly)?;
    let span = Span::new(start_token.span.start(), close_token.span.end());

    Ok(Expr {
        kind: ExprKind::ArrayLiteral(ArrayLiteralExpr {
            underlying: type_,
            contents,
        }),
        span,
    })
}

pub fn parse_function_call_expr(
    parser: &mut Parser,
    left: Expr,
    _bp: BindingPower,
) -> Result<Expr> {
    parser.expect(TokenKind::OpenParen)?;

    let mut arguments: ThinVec<Expr> = ThinVec::new();

    loop {
        if parser.current_token().kind == TokenKind::CloseParen {
            break;
        }

        arguments.push(parse_expr(parser, BindingPower::Assignment)?);

        if parser.current_token().kind != TokenKind::CloseParen {
            parser.expect(TokenKind::Comma)?;
        }
    }

    parser.expect(TokenKind::CloseParen)?;

    let span = Span::new(left.span.start(), parser.current_token().span.end());
    Ok(Expr {
        kind: ExprKind::FunctionCall(FunctionCallExpr {
            callee: Box::new(left),
            arguments,
        }),
        span,
    })
}

pub fn parse_member_access_expr(
    parser: &mut Parser,
    left: Expr,
    _bp: BindingPower,
) -> Result<Expr> {
    let operator = parser.advance();

    let member = parser.expect_identifier()?;
    let member_span = member.span;

    let span = Span::new(left.span.start(), member_span.end());
    Ok(Expr {
        kind: ExprKind::MemberAccess(MemberAccessExpr {
            base: Box::new(left),
            member,
            operator,
        }),
        span,
    })
}

pub fn parse_type_expr(parser: &mut Parser) -> Result<Expr> {
    let start_token = parser.expect(TokenKind::Dollar)?;

    let ty = parse_type(parser, BindingPower::DefaultBp)?;

    let end_token = parser.current_token();
    let span = Span::new(start_token.span.start(), end_token.span.end());

    Ok(Expr {
        kind: ExprKind::Type(TypeExpr { underlying: ty }),
        span,
    })
}

pub fn parse_as_cast_expr(parser: &mut Parser, left: Expr, _bp: BindingPower) -> Result<Expr> {
    parser.expect(TokenKind::As)?;

    let ty = parse_type(parser, BindingPower::DefaultBp)?;

    let span = Span::new(left.span.start(), ty.span.end());
    Ok(Expr {
        kind: ExprKind::As(AsExpr {
            expr: Box::new(left),
            ty,
        }),
        span,
    })
}

pub fn parse_parenthesis_expr(parser: &mut Parser) -> Result<Expr> {
    let start_token = parser.expect(TokenKind::OpenParen)?;

    let mut elements = ThinVec::new();
    let mut has_comma = false;

    while parser.current_token().kind != TokenKind::CloseParen {
        elements.push(parse_expr(parser, BindingPower::DefaultBp)?);

        if parser.current_token().kind == TokenKind::Comma {
            has_comma = true;
            parser.advance();
        } else if parser.current_token().kind != TokenKind::CloseParen {
            fatal_at!(
                parser.current_token().span,
                parser.current_token().module_id,
                "Expected comma or closing parenthesis in expression"
            );
        }
    }
    let close_token = parser.expect(TokenKind::CloseParen)?;
    let end_span = close_token.span;

    if elements.len() == 1 && !has_comma {
        let mut expr = elements.pop().expect("expressions isn't empty");
        expr.span = Span::new(start_token.span.start(), end_span.end());
        Ok(expr)
    } else {
        Ok(Expr {
            kind: ExprKind::TupleLiteral(TupleLiteralExpr { elements }),
            span: Span::new(start_token.span.start(), end_span.end()),
        })
    }
}

pub fn parse_block_expr(parser: &mut Parser) -> Result<Expr> {
    let start_token = parser.expect(TokenKind::OpenCurly)?;
    let mut body = ThinVec::new();

    loop {
        if parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        body.push(parse_stmt(parser)?);
    }

    let end_token = parser.expect(TokenKind::CloseCurly)?;
    let span = Span::new(start_token.span.start(), end_token.span.end());

    Ok(Expr {
        kind: ExprKind::Block(BlockExpr {
            block: Block { body },
        }),
        span,
    })
}
