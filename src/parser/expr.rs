use std::collections::HashMap;

use colored::Colorize;

use crate::{
    ast::{
        ast::Expr,
        expressions::{
            ArrayLiteralExpr, AssignmentExpr, BinaryExpr, FixedArrayLiteralExpr, NumberExpr,
            PrefixExpr, StringExpr, StructInstantiationExpr, SymbolExpr,
        },
    },
    lexer::token::TokenKind,
    parser::lookups::LED_LU,
};

use super::{
    lookups::{BP_LU, BindingPower, NUD_LU},
    parser::Parser,
    types::parse_type,
};

pub fn parse_expr(parser: &mut Parser, bp: BindingPower) -> Box<dyn Expr> {
    let token_kind = parser.current_token_kind();

    let nud_fn = {
        let nud_lu = NUD_LU.lock().unwrap();
        nud_lu.get(&token_kind).cloned().unwrap_or_else(|| {
            panic!(
                "{}",
                format!("Nud handler expected for token {:?}", token_kind)
                    .red()
                    .bold()
            )
        })
    };

    let mut left = nud_fn(parser);

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
            led_lu.get(&token_kind).cloned().unwrap_or_else(|| {
                panic!(
                    "{}",
                    format!("Led handler expected for token {:?}", token_kind)
                        .red()
                        .bold()
                )
            })
        };

        left = led_fn(parser, left.clone(), current_bp);
    }

    left
}

pub fn parse_primary_expr(parser: &mut Parser) -> Box<dyn Expr> {
    match parser.current_token_kind() {
        TokenKind::NumberLiteral => {
            let value = parser.advance().value;
            // Default to f32 for backward compatibility
            Box::new(NumberExpr {
                value: value.parse::<f32>().unwrap(),
            })
        }
        TokenKind::FloatLiteral => {
            let value = parser.advance().value;
            // Parse as f64 for better precision
            Box::new(NumberExpr {
                value: value.parse::<f64>().unwrap(),
            })
        }
        TokenKind::IntegerLiteral => {
            let value = parser.advance().value;
            // Default to i32 for integers
            Box::new(NumberExpr {
                value: value.parse::<i32>().unwrap(),
            })
        }
        TokenKind::UnsignedLiteral => {
            let value = parser.advance().value;
            // Default to u32 for unsigned integers
            Box::new(NumberExpr {
                value: value.parse::<u32>().unwrap(),
            })
        }
        TokenKind::StringLiteral => Box::new(StringExpr {
            value: parser.advance().value,
        }),
        TokenKind::Identifier => Box::new(SymbolExpr {
            value: parser.advance().value,
        }),
        _ => panic!(
            "{}",
            format!(
                "Cannot create primary expression from {:?}",
                parser.current_token_kind()
            )
            .red()
            .bold()
        ),
    }
}

pub fn parse_binary_expr(
    parser: &mut Parser,
    left: Box<dyn Expr>,
    bp: BindingPower,
) -> Box<dyn Expr> {
    let operator = parser.advance();
    let right = parse_expr(parser, bp);

    Box::new(BinaryExpr {
        left,
        operator,
        right,
    })
}

pub fn parse_prefix_expr(parser: &mut Parser) -> Box<dyn Expr> {
    let operator = parser.advance();
    let right = parse_expr(parser, BindingPower::DefaultBp);

    Box::new(PrefixExpr { operator, right })
}

pub fn parse_assignment_expr(
    parser: &mut Parser,
    assigne: Box<dyn Expr>,
    bp: BindingPower,
) -> Box<dyn Expr> {
    let operator = parser.advance();
    let value = parse_expr(parser, BindingPower::Assignment);

    Box::new(AssignmentExpr {
        assigne,
        operator,
        value,
    })
}

pub fn parse_grouping_expr(parser: &mut Parser) -> Box<dyn Expr> {
    parser.advance();
    let expr = parse_expr(parser, BindingPower::DefaultBp);
    parser.expect(TokenKind::CloseParen);

    expr
}

pub fn parse_struct_instantiation_expr(
    parser: &mut Parser,
    left: Box<dyn Expr>,
    bp: BindingPower,
) -> Box<dyn Expr> {
    // Reflection or something
    let name = match left.as_any().downcast_ref::<SymbolExpr>() {
        Some(symbol) => symbol.value.clone(),
        None => panic!("Expected struct name for struct instantiation"),
    };
    let mut properties: HashMap<String, Box<dyn Expr>> = HashMap::new();

    parser.expect(TokenKind::OpenCurly);

    loop {
        if !parser.has_tokens() || parser.current_token_kind() == TokenKind::CloseCurly {
            break;
        }

        let property_name = parser.expect(TokenKind::Identifier).value;
        parser.expect(TokenKind::Colon);
        let expr = parse_expr(parser, BindingPower::Logical);

        if parser.current_token_kind() != TokenKind::CloseCurly {
            parser.expect(TokenKind::Comma);
        }

        if properties.contains_key(&property_name) {
            panic!(
                "{}",
                format!(
                    "Property {} has already been defined in struct instantiation",
                    property_name
                )
                .red()
                .bold()
            );
        }

        properties.insert(property_name, expr);
    }

    parser.expect(TokenKind::CloseCurly);

    Box::new(StructInstantiationExpr { name, properties })
}

pub fn parse_array_literal_expr(parser: &mut Parser) -> Box<dyn Expr> {
    parser.advance();

    match parser.current_token_kind() {
        TokenKind::NumberLiteral | TokenKind::IntegerLiteral | TokenKind::UnsignedLiteral => {
            let length = parser.current_token().value.parse::<usize>().unwrap();
            parser.advance();
            parser.expect(TokenKind::CloseBracket);
            let underlying = parse_type(parser, BindingPower::DefaultBp);

            parser.expect(TokenKind::OpenCurly);
            let mut contents = Vec::with_capacity(length);

            loop {
                if !parser.has_tokens() || parser.current_token_kind() == TokenKind::CloseCurly {
                    break;
                }

                contents.push(parse_expr(parser, BindingPower::Logical));

                if parser.current_token_kind() != TokenKind::CloseCurly {
                    parser.expect(TokenKind::Comma);
                }
            }
            parser.expect(TokenKind::CloseCurly);

            if contents.len() != length {
                panic!(
                    "{}",
                    format!(
                        "Fixed array literal has {} elements but expected {}",
                        contents.len(),
                        length
                    )
                    .red()
                    .bold()
                );
            }

            Box::new(FixedArrayLiteralExpr {
                underlying,
                length,
                contents,
            })
        }
        TokenKind::CloseBracket => {
            parser.advance();
            let underlying = parse_type(parser, BindingPower::DefaultBp);

            parser.expect(TokenKind::OpenCurly);
            let mut contents = Vec::new();

            loop {
                if !parser.has_tokens() || parser.current_token_kind() == TokenKind::CloseCurly {
                    break;
                }

                contents.push(parse_expr(parser, BindingPower::Logical));

                if parser.current_token_kind() != TokenKind::CloseCurly {
                    parser.expect(TokenKind::Comma);
                }
            }
            parser.expect(TokenKind::CloseCurly);

            Box::new(ArrayLiteralExpr {
                underlying,
                contents,
            })
        }
        _ => panic!(
            "{}",
            format!(
                "Expected number or ']' in array literal, got {:?}",
                parser.current_token_kind()
            )
            .red()
            .bold()
        ),
    }
}
