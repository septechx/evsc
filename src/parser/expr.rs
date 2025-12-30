use std::collections::HashMap;

use anyhow::{Result, bail};
use colored::Colorize;

use crate::{
    ast::{
        Expr, ExprKind,
        expressions::{
            ArrayLiteralExpr, AssignmentExpr, BinaryExpr, FunctionCallExpr, MemberAccessExpr,
            NumberExpr, PrefixExpr, StringExpr, StructInstantiationExpr, SymbolExpr, TypeExpr,
        },
    },
    errors::{CodeLine, CodeType, builders},
    lexer::token::{Token, TokenKind},
    parser::{
        Parser,
        lookups::{BP_LU, BindingPower, LED_LU, NUD_LU},
        string::process_string,
        types::parse_type,
    },
    span::Span,
};

fn handle_unexpected_token(_parser: &mut Parser, token: Token) -> ! {
    let span = token.span;
    let module_id = token.module_id;

    let (_, line, ..) = crate::SOURCE_MAPS.with(|sm| {
        let maps = sm.borrow();
        maps.get_source(module_id)
            .map(|sm| sm.span_to_source_location(&span))
            .unwrap_or(Default::default())
    });

    let line_content = crate::SOURCE_MAPS.with(|sm| {
        let maps = sm.borrow();
        maps.get_source(token.module_id)
            .and_then(|sm| sm.get_line(line))
            .unwrap_or("")
            .to_string()
    });

    crate::ERRORS.with(|e| {
        e.borrow_mut().add(
            builders::fatal(format!("Syntax error: Unexpected token `{}`", token.value))
                .with_span(span, module_id)
                .with_code(CodeLine::new(line, line_content, CodeType::None)),
        );
    });

    unreachable!()
}

pub fn parse_expr(parser: &mut Parser, bp: BindingPower) -> Result<Expr> {
    let token = parser.current_token();
    let start_span = token.span;

    let nud_fn = {
        let nud_lu = NUD_LU.lock();
        nud_lu
            .get(&token.kind)
            .cloned()
            .unwrap_or_else(|| handle_unexpected_token(parser, token.clone()))
    };

    let mut left = nud_fn(parser)?;

    let mut end_span = left.span;

    loop {
        let current_bp = {
            let bp_lu = BP_LU.lock();
            *bp_lu
                .get(&parser.current_token().kind)
                .unwrap_or(&BindingPower::DefaultBp)
        };

        if current_bp <= bp {
            break;
        }

        let token_kind = parser.current_token();
        let led_fn = {
            let led_lu = LED_LU.lock();
            led_lu
                .get(&token_kind.kind)
                .cloned()
                .unwrap_or_else(|| handle_unexpected_token(parser, token_kind.clone()))
        };

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
        TokenKind::Number => Ok(parser.expr(
            ExprKind::Number(NumberExpr {
                value: value.parse::<i32>()?,
            }),
            span,
        )),
        TokenKind::StringLiteral => Ok(parser.expr(
            ExprKind::String(StringExpr {
                value: process_string(&value, span, token.module_id),
            }),
            span,
        )),
        TokenKind::Identifier => Ok(parser.expr(ExprKind::Symbol(SymbolExpr { value }), span)),
        _ => bail!(
            format!(
                "Cannot create primary expression from {:?}",
                parser.current_token()
            )
            .red()
            .bold()
        ),
    }
}

pub fn parse_binary_expr(parser: &mut Parser, left: Expr, bp: BindingPower) -> Result<Expr> {
    let operator = parser.advance();
    let right = parse_expr(parser, bp)?;

    let span = Span::new(left.span.start(), right.span.end());
    Ok(parser.expr(
        ExprKind::Binary(BinaryExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }),
        span,
    ))
}

pub fn parse_prefix_expr(parser: &mut Parser) -> Result<Expr> {
    let operator = parser.advance();
    let right = parse_expr(parser, BindingPower::DefaultBp)?;

    let span = Span::new(operator.span.start(), right.span.end());
    Ok(parser.expr(
        ExprKind::Prefix(PrefixExpr {
            operator,
            right: Box::new(right),
        }),
        span,
    ))
}

pub fn parse_assignment_expr(
    parser: &mut Parser,
    assigne: Expr,
    _bp: BindingPower,
) -> Result<Expr> {
    let operator = parser.advance();
    let value = parse_expr(parser, BindingPower::Assignment)?;

    let span = Span::new(assigne.span.start(), value.span.end());
    Ok(parser.expr(
        ExprKind::Assignment(AssignmentExpr {
            assigne: Box::new(assigne),
            operator,
            value: Box::new(value),
        }),
        span,
    ))
}

pub fn parse_grouping_expr(parser: &mut Parser) -> Result<Expr> {
    parser.expect(TokenKind::OpenParen)?;
    let expr = parse_expr(parser, BindingPower::DefaultBp)?;
    parser.expect(TokenKind::CloseParen)?;
    Ok(expr)
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

    let mut properties: HashMap<String, Expr> = HashMap::new();

    loop {
        if parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        let property_name = parser.expect(TokenKind::Identifier)?.value;
        parser.expect(TokenKind::Colon)?;
        let value = parse_expr(parser, BindingPower::Assignment)?;

        properties.insert(property_name, value);

        if parser.current_token().kind != TokenKind::CloseCurly {
            parser.expect(TokenKind::Comma)?;
        }
    }

    let close_token = parser.expect(TokenKind::CloseCurly)?;

    let span = Span::new(left.span.start(), close_token.span.end());
    Ok(parser.expr(
        ExprKind::StructInstantiation(StructInstantiationExpr {
            name: struct_name,
            properties,
        }),
        span,
    ))
}

pub fn parse_array_literal_expr(parser: &mut Parser) -> Result<Expr> {
    let start_token = parser.expect(TokenKind::OpenBracket)?;
    parser.expect(TokenKind::CloseBracket)?;

    let type_ = parse_type(parser, BindingPower::DefaultBp)?;

    parser.expect(TokenKind::OpenCurly)?;

    let mut contents: Vec<Expr> = Vec::new();

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

    Ok(parser.expr(
        ExprKind::ArrayLiteral(ArrayLiteralExpr {
            underlying: type_,
            contents,
        }),
        span,
    ))
}

pub fn parse_function_call_expr(
    parser: &mut Parser,
    left: Expr,
    _bp: BindingPower,
) -> Result<Expr> {
    parser.expect(TokenKind::OpenParen)?;

    let mut arguments: Vec<Expr> = Vec::new();

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
    Ok(parser.expr(
        ExprKind::FunctionCall(FunctionCallExpr {
            callee: Box::new(left),
            arguments,
        }),
        span,
    ))
}

pub fn parse_member_access_expr(
    parser: &mut Parser,
    left: Expr,
    _bp: BindingPower,
) -> Result<Expr> {
    parser.expect(TokenKind::Dot)?;

    let member_token = parser.expect(TokenKind::Identifier)?;
    let member = SymbolExpr {
        value: member_token.value,
    };

    let span = Span::new(left.span.start(), member_token.span.end());
    Ok(parser.expr(
        ExprKind::MemberAccess(MemberAccessExpr {
            base: Box::new(left),
            member,
        }),
        span,
    ))
}

pub fn parse_type_expr(parser: &mut Parser) -> Result<Expr> {
    let start_token = parser.expect(TokenKind::Dollar)?;

    let ty = parse_type(parser, BindingPower::DefaultBp)?;

    let end_token = parser.current_token();
    let span = Span::new(start_token.span.start(), end_token.span.end());

    Ok(parser.expr(ExprKind::Type(TypeExpr { underlying: ty }), span))
}
