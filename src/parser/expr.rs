use colored::Colorize;

use crate::{
    ast::{
        ast::Expr,
        expressions::{BinaryExpr, NumberExpr, StringExpr, SymbolExpr},
    },
    lexer::token::TokenKind,
    parser::lookups::LED_LU,
};

use super::{
    lookups::{BP_LU, BindingPower, NUD_LU},
    parser::Parser,
};

// 42 + 5 * 4;
pub fn parse_expr(parser: &mut Parser, bp: BindingPower) -> Box<dyn Expr> {
    let token_kind = parser.current_token_kind();

    let nud_fn = {
        let nud_lu = NUD_LU.lock().unwrap();
        nud_lu.get(&token_kind).cloned().unwrap_or_else(|| {
            panic!(
                "{}",
                format!(
                    "[Parser/ERROR] Nud handler expected for token {:?}",
                    token_kind
                )
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
                    format!(
                        "[Parser/ERROR] Led handler expected for token {:?}",
                        token_kind
                    )
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
        TokenKind::Number => Box::new(NumberExpr {
            value: parser.advance().value.parse::<f32>().unwrap(),
        }),
        TokenKind::StringLiteral => Box::new(StringExpr {
            value: parser.advance().value,
        }),
        TokenKind::Identifier => Box::new(SymbolExpr {
            value: parser.advance().value,
        }),
        _ => panic!(
            "Cannot create primary expression from {:?}",
            parser.current_token_kind()
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
