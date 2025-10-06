use std::collections::HashMap;

use anyhow::{anyhow, bail, Result};
use colored::Colorize;

use crate::{
    ast::{
        ast::Expression,
        expressions::{
            ArrayLiteralExpr, AssignmentExpr, BinaryExpr, FixedArrayLiteralExpr, FunctionCallExpr,
            MemberAccessExpr, NumberExpr, PrefixExpr, StringExpr, StructInstantiationExpr,
            SymbolExpr, TypeExpr,
        },
    },
    errors::{CodeLine, CodeType, CompilationError, ErrorLevel},
    lexer::{
        token::{Token, TokenKind},
        verify::build_line_with_positions,
    },
    parser::{
        lookups::{BindingPower, BP_LU, LED_LU, NUD_LU},
        parser::Parser,
        string::process_string,
        types::parse_type,
    },
    ERRORS,
};

fn handle_unexpected_token(parser: &mut Parser, token: Token) -> ! {
    ERRORS.lock().add(
        CompilationError::new(
            ErrorLevel::Fatal,
            format!("Syntax error: Unexpected token `{}`", token.value),
        )
        .with_location(parser.current_token().location.clone())
        .with_code(CodeLine::new(
            token.location.line,
            build_line_with_positions(parser.tokens(), token.location.line),
            CodeType::None,
        )),
    );

    unreachable!()
}

pub fn parse_expr(parser: &mut Parser, bp: BindingPower) -> Result<Expression> {
    let token = parser.current_token();

    let nud_fn = {
        let nud_lu = NUD_LU.lock().unwrap();
        nud_lu
            .get(&token.kind)
            .cloned()
            .unwrap_or_else(|| handle_unexpected_token(parser, token.clone()))
    };

    let mut left = nud_fn(parser)?;

    loop {
        let current_bp = {
            let bp_lu = BP_LU.lock().unwrap();
            *bp_lu
                .get(&parser.current_token().kind)
                .unwrap_or(&BindingPower::DefaultBp)
        };

        if current_bp <= bp {
            break;
        }

        let token_kind = parser.current_token();
        let led_fn = {
            let led_lu = LED_LU.lock().unwrap();
            led_lu
                .get(&token_kind.kind)
                .cloned()
                .unwrap_or_else(|| handle_unexpected_token(parser, token_kind.clone()))
        };

        left = led_fn(parser, left.clone(), current_bp)?;
    }

    Ok(left)
}

pub fn parse_primary_expr(parser: &mut Parser) -> Result<Expression> {
    let value = parser.current_token().value.clone();
    match parser.current_token().kind {
        TokenKind::Number => {
            parser.advance();
            Ok(Expression::Number(NumberExpr {
                value: value.parse::<i32>()?,
            }))
        }
        TokenKind::StringLiteral => {
            let token = parser.advance();
            Ok(Expression::String(StringExpr {
                value: process_string(&value, token.location, parser.tokens()),
            }))
        }
        TokenKind::Identifier => {
            parser.advance();
            Ok(Expression::Symbol(SymbolExpr { value }))
        }
        _ => bail!(format!(
            "Cannot create primary expression from {:?}",
            parser.current_token()
        )
        .red()
        .bold()),
    }
}

pub fn parse_binary_expr(
    parser: &mut Parser,
    left: Expression,
    bp: BindingPower,
) -> Result<Expression> {
    let operator = parser.advance();
    let right = parse_expr(parser, bp)?;

    Ok(Expression::Binary(BinaryExpr {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    }))
}

pub fn parse_prefix_expr(parser: &mut Parser) -> Result<Expression> {
    let operator = parser.advance();
    let right = parse_expr(parser, BindingPower::DefaultBp)?;

    Ok(Expression::Prefix(PrefixExpr {
        operator,
        right: Box::new(right),
    }))
}

pub fn parse_assignment_expr(
    parser: &mut Parser,
    assigne: Expression,
    _bp: BindingPower,
) -> Result<Expression> {
    let operator = parser.advance();
    let value = parse_expr(parser, BindingPower::Assignment)?;

    Ok(Expression::Assignment(AssignmentExpr {
        assigne: Box::new(assigne),
        operator,
        value: Box::new(value),
    }))
}

pub fn parse_grouping_expr(parser: &mut Parser) -> Result<Expression> {
    parser.advance();
    let expr = parse_expr(parser, BindingPower::DefaultBp)?;
    parser.expect(TokenKind::CloseParen)?;

    Ok(expr)
}

pub fn parse_struct_instantiation_expr(
    parser: &mut Parser,
    name_expr: Expression,
    _bp: BindingPower,
) -> Result<Expression> {
    parser.advance();

    let name = match name_expr {
        Expression::Symbol(symbol_expr) => symbol_expr.value,
        _ => return Err(anyhow!("Expected struct name to be a symbol")),
    };

    let mut properties: HashMap<String, Expression> = HashMap::new();

    loop {
        if parser.current_token().kind == TokenKind::CloseCurly {
            parser.advance();
            break;
        }

        let property_name = parser.expect(TokenKind::Identifier)?.value;
        parser.expect(TokenKind::Colon)?;

        let expr = parse_expr(parser, BindingPower::DefaultBp)?;

        properties.insert(property_name, expr);

        if parser.current_token().kind == TokenKind::CloseCurly {
            parser.advance();
            break;
        }

        parser.expect(TokenKind::Comma)?;
    }

    Ok(Expression::StructInstantiation(StructInstantiationExpr {
        name,
        properties,
    }))
}

pub fn parse_function_call_expr(
    parser: &mut Parser,
    callee: Expression,
    _bp: BindingPower,
) -> Result<Expression> {
    parser.advance();

    let mut arguments: Vec<Expression> = Vec::new();

    loop {
        if parser.current_token().kind == TokenKind::CloseParen {
            parser.advance();
            break;
        }

        let expr = parse_expr(parser, BindingPower::DefaultBp)?;

        arguments.push(expr);

        if parser.current_token().kind == TokenKind::CloseParen {
            parser.advance();
            break;
        }

        parser.expect(TokenKind::Comma)?;
    }

    Ok(Expression::FunctionCall(FunctionCallExpr {
        callee: Box::new(callee),
        arguments,
    }))
}

pub fn parse_array_literal_expr(parser: &mut Parser) -> Result<Expression> {
    parser.advance();

    match parser.current_token().kind {
        TokenKind::Number => {
            let length = parser.current_token().value.parse::<usize>()?;

            // Fixed-size array
            parser.advance();
            parser.expect(TokenKind::CloseBracket)?;

            let underlying = parse_type(parser, BindingPower::DefaultBp)?;

            parser.expect(TokenKind::OpenCurly)?;
            let mut contents = Vec::with_capacity(length);

            while parser.current_token().kind != TokenKind::CloseCurly {
                contents.push(parse_expr(parser, BindingPower::Logical)?);

                if parser.current_token().kind != TokenKind::CloseCurly {
                    parser.expect(TokenKind::Comma)?;
                }
            }
            parser.expect(TokenKind::CloseCurly)?;

            if contents.len() != length {
                bail!(format!(
                    "Fixed array literal has {} elements but expected {}",
                    contents.len(),
                    length
                )
                .red()
                .bold());
            }

            Ok(Expression::FixedArrayLiteral(FixedArrayLiteralExpr {
                underlying,
                length,
                contents,
            }))
        }
        TokenKind::CloseBracket => {
            // Variable-length array
            parser.advance();
            let underlying = parse_type(parser, BindingPower::DefaultBp)?;

            parser.expect(TokenKind::OpenCurly)?;
            let mut contents = Vec::new();

            while parser.current_token().kind != TokenKind::CloseCurly {
                contents.push(parse_expr(parser, BindingPower::Logical)?);

                if parser.current_token().kind != TokenKind::CloseCurly {
                    parser.expect(TokenKind::Comma)?;
                }
            }
            parser.expect(TokenKind::CloseCurly)?;

            Ok(Expression::ArrayLiteral(ArrayLiteralExpr {
                underlying,
                contents,
            }))
        }
        _ => Err(anyhow!(format!(
            "Expected number or ']' in array literal, got {:?}",
            parser.current_token()
        )
        .red()
        .bold())),
    }
}

pub fn parse_member_access_expr(
    parser: &mut Parser,
    base: Expression,
    _bp: BindingPower,
) -> Result<Expression> {
    parser.expect(TokenKind::Dot)?;
    let member = SymbolExpr {
        value: parser.expect(TokenKind::Identifier)?.value,
    };
    Ok(Expression::MemberAccess(MemberAccessExpr {
        base: Box::new(base),
        member,
    }))
}

pub fn parse_type_expr(parser: &mut Parser) -> Result<Expression> {
    parser.expect(TokenKind::Dollar)?;

    let underlying = parse_type(parser, BindingPower::DefaultBp)?;
    Ok(Expression::Type(TypeExpr { underlying }))
}
