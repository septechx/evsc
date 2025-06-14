use std::{collections::HashMap, sync::Mutex};

use colored::Colorize;
use lazy_static::lazy_static;

use crate::{
    ast::{
        ast::Type,
        types::{SymbolType, VectorType},
    },
    lexer::token::TokenKind::{self, *},
};

use super::{
    lookups::{
        BindingPower::{self, *},
        BpLookup,
    },
    parser::Parser,
};

type TypeNudHandler = fn(&mut Parser) -> Box<dyn Type>;
type TypeLedHandler = fn(&mut Parser, Box<dyn Type>, BindingPower) -> Box<dyn Type>;

type TypeNudLookup = HashMap<TokenKind, TypeNudHandler>;
type TypeLedLookup = HashMap<TokenKind, TypeLedHandler>;

lazy_static! {
    pub static ref TYPE_BP_LU: Mutex<BpLookup> = Mutex::new(HashMap::new());
    pub static ref TYPE_NUD_LU: Mutex<TypeNudLookup> = Mutex::new(HashMap::new());
    pub static ref TYPE_LED_LU: Mutex<TypeLedLookup> = Mutex::new(HashMap::new());
}

fn type_led(kind: TokenKind, bp: BindingPower, led_fn: TypeLedHandler) {
    TYPE_BP_LU.lock().unwrap().insert(kind, bp);
    TYPE_LED_LU.lock().unwrap().insert(kind, led_fn);
}

fn type_nud(kind: TokenKind, nud_fn: TypeNudHandler) {
    TYPE_NUD_LU.lock().unwrap().insert(kind, nud_fn);
}

pub fn create_token_type_lookups() {
    type_nud(Identifier, parse_symbol_type);
    type_nud(OpenBracket, parse_array_type)
}

fn parse_symbol_type(parser: &mut Parser) -> Box<dyn Type> {
    Box::new(SymbolType {
        name: parser.expect(Identifier).value,
    })
}

// TODO: Add support for [len]type for fixed size arrays
fn parse_array_type(parser: &mut Parser) -> Box<dyn Type> {
    parser.advance();
    parser.expect(TokenKind::CloseBracket);
    let underlying = parse_type(parser, DefaultBp);
    Box::new(VectorType { underlying })
}

pub fn parse_type(parser: &mut Parser, bp: BindingPower) -> Box<dyn Type> {
    let token_kind = parser.current_token_kind();

    let nud_fn = {
        let nud_lu = TYPE_NUD_LU.lock().unwrap();
        nud_lu.get(&token_kind).cloned().unwrap_or_else(|| {
            panic!(
                "{}",
                format!("Type nud handler expected for token {:?}", token_kind)
                    .red()
                    .bold()
            )
        })
    };

    let mut left = nud_fn(parser);

    loop {
        let current_bp = {
            let bp_lu = TYPE_BP_LU.lock().unwrap();
            *bp_lu
                .get(&parser.current_token_kind())
                .unwrap_or(&BindingPower::DefaultBp)
        };

        if current_bp <= bp {
            break;
        }

        let token_kind = parser.current_token_kind();
        let led_fn = {
            let led_lu = TYPE_LED_LU.lock().unwrap();
            led_lu.get(&token_kind).cloned().unwrap_or_else(|| {
                panic!(
                    "{}",
                    format!("Type led handler expected for token {:?}", token_kind)
                        .red()
                        .bold()
                )
            })
        };

        left = led_fn(parser, left.clone(), current_bp);
    }

    left
}
