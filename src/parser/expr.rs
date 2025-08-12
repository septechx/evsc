use std::collections::HashMap;

use anyhow::{anyhow, Result};
use colored::Colorize;

use crate::{
    ast::{
        ast::Expression,
        expressions::{
            ArrayLiteralExpr, AssignmentExpr, BinaryExpr, FixedArrayLiteralExpr, FunctionCallExpr,
            MemberAccessExpr, NumberExpr, PrefixExpr, StringExpr, StructInstantiationExpr,
            SymbolExpr,
        },
    },
    lexer::token::TokenKind,
    parser::lookups::LED_LU,
};

use super::{
    lookups::{BindingPower, BP_LU, NUD_LU},
    parser::Parser,
    types::parse_type,
};

pub fn parse_expr(parser: &mut Parser, bp: BindingPower) -> Result<Expression> {
    let token_kind = parser.current_token_kind();

    let nud_fn = {
        let nud_lu = NUD_LU.lock().unwrap();
        nud_lu.get(&token_kind).cloned().ok_or_else(|| {
            anyhow!(
                "{}",
                format!("Nud handler expected for token {token_kind:?}")
                    .red()
                    .bold()
            )
        })?
    };

    let mut left = nud_fn(parser)?;

    loop {
        let current_bp = {
            let bp_lu = BP_LU.lock().unwrap();
            *bp_lu
                .get(&parser.current_token_kind())
                .unwrap_or(&BindingPower::DefaultBp)
        };

        if current_bp <= bp {
            break;
        }

        let token_kind = parser.current_token_kind();
        let led_fn = {
            let led_lu = LED_LU.lock().unwrap();
            led_lu.get(&token_kind).cloned().ok_or_else(|| {
                anyhow!(
                    "{}",
                    format!("Led handler expected for token {token_kind:?}")
                        .red()
                        .bold()
                )
            })?
        };

        left = led_fn(parser, left.clone(), current_bp)?;
    }

    Ok(left)
}

pub fn parse_primary_expr(parser: &mut Parser) -> Result<Expression> {
    match parser.current_token_kind() {
        TokenKind::Number => {
            let value = parser.advance().value;
            Ok(Expression::Number(NumberExpr {
                value: value
                    .parse::<i32>()
                    .map_err(|e| anyhow!("Failed to parse number: {}", e))?,
            }))
        }
        TokenKind::StringLiteral => Ok(Expression::String(StringExpr {
            value: parser.advance().value,
        })),
        TokenKind::Identifier => Ok(Expression::Symbol(SymbolExpr {
            value: parser.advance().value,
        })),
        _ => Err(anyhow!(
            "{}",
            format!(
                "Cannot create primary expression from {:?}",
                parser.current_token_kind()
            )
            .red()
            .bold()
        )),
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
        if parser.current_token_kind() == TokenKind::CloseCurly {
            parser.advance();
            break;
        }

        let property_name = parser.expect(TokenKind::Identifier)?.value;
        parser.expect(TokenKind::Colon)?;

        let expr = parse_expr(parser, BindingPower::DefaultBp)?;

        properties.insert(property_name, expr);

        if parser.current_token_kind() == TokenKind::CloseCurly {
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
        if parser.current_token_kind() == TokenKind::CloseParen {
            parser.advance();
            break;
        }

        let expr = parse_expr(parser, BindingPower::DefaultBp)?;

        arguments.push(expr);

        if parser.current_token_kind() == TokenKind::CloseParen {
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

    match parser.current_token_kind() {
        TokenKind::Number => {
            // Fixed-size array
            let length = parser
                .current_token()
                .value
                .parse::<usize>()
                .map_err(|e| anyhow!("Failed to parse array length: {}", e))?;
            parser.advance();
            parser.expect(TokenKind::CloseBracket)?;

            // Get the underlying type
            let underlying = parse_type(parser, BindingPower::DefaultBp)?;

            // Parse array contents
            parser.expect(TokenKind::OpenCurly)?;
            let mut contents = Vec::with_capacity(length);

            while parser.current_token_kind() != TokenKind::CloseCurly {
                contents.push(parse_expr(parser, BindingPower::Logical)?);

                if parser.current_token_kind() != TokenKind::CloseCurly {
                    parser.expect(TokenKind::Comma)?;
                }
            }
            parser.expect(TokenKind::CloseCurly)?;

            if contents.len() != length {
                return Err(anyhow!(
                    "{}",
                    format!(
                        "Fixed array literal has {} elements but expected {}",
                        contents.len(),
                        length
                    )
                    .red()
                    .bold()
                ));
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

            while parser.current_token_kind() != TokenKind::CloseCurly {
                contents.push(parse_expr(parser, BindingPower::Logical)?);

                if parser.current_token_kind() != TokenKind::CloseCurly {
                    parser.expect(TokenKind::Comma)?;
                }
            }
            parser.expect(TokenKind::CloseCurly)?;

            Ok(Expression::ArrayLiteral(ArrayLiteralExpr {
                underlying,
                contents,
            }))
        }
        _ => Err(anyhow!(
            "{}",
            format!(
                "Expected number or ']' in array literal, got {:?}",
                parser.current_token_kind()
            )
            .red()
            .bold()
        )),
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
