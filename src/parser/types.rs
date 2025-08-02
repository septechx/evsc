use std::{collections::HashMap, sync::Mutex};

use colored::Colorize;
use lazy_static::lazy_static;

use crate::{
    ast::{
        ast::Type,
        types::{FixedArrayType, SliceType, SymbolType},
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

type TypeNudHandler = fn(&mut Parser) -> anyhow::Result<Type>;
type TypeLedHandler = fn(&mut Parser, Type, BindingPower) -> anyhow::Result<Type>;

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

fn parse_symbol_type(parser: &mut Parser) -> anyhow::Result<Type> {
    Ok(Type::Symbol(SymbolType {
        name: parser.expect(Identifier)?.value,
    }))
}

fn parse_array_type(parser: &mut Parser) -> anyhow::Result<Type> {
    parser.advance();

    match parser.current_token_kind() {
        Number => {
            let length = parser
                .current_token()
                .value
                .parse::<usize>()
                .map_err(|e| anyhow::anyhow!("Failed to parse array length: {}", e))?;
            parser.advance();
            parser.expect(CloseBracket)?;
            let underlying = parse_type(parser, DefaultBp)?;
            Ok(Type::FixedArray(FixedArrayType {
                length,
                underlying: Box::new(underlying),
            }))
        }
        CloseBracket => {
            parser.advance();
            let underlying = parse_type(parser, DefaultBp)?;
            Ok(Type::Slice(SliceType {
                underlying: Box::new(underlying),
            }))
        }
        _ => Err(anyhow::anyhow!(
            "{}",
            format!(
                "Expected number or ']' in array type, got {:?}",
                parser.current_token_kind()
            )
            .red()
            .bold()
        )),
    }
}

pub fn parse_type(parser: &mut Parser, bp: BindingPower) -> anyhow::Result<Type> {
    let token_kind = parser.current_token_kind();

    let nud_fn = {
        let nud_lu = TYPE_NUD_LU.lock().unwrap();
        nud_lu.get(&token_kind).cloned().ok_or_else(|| {
            anyhow::anyhow!(
                "{}",
                format!("Type nud handler expected for token {:?}", token_kind)
                    .red()
                    .bold()
            )
        })?
    };

    let mut left = nud_fn(parser)?;

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
            led_lu.get(&token_kind).cloned().ok_or_else(|| {
                anyhow::anyhow!(
                    "{}",
                    format!("Type led handler expected for token {:?}", token_kind)
                        .red()
                        .bold()
                )
            })?
        };

        left = led_fn(parser, left, current_bp)?;
    }

    Ok(left)
}
