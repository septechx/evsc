use anyhow::{Result, anyhow, bail};
use parking_lot::Once;
use std::{collections::HashMap, sync::OnceLock};

use colored::Colorize;

use crate::{
    ast::{
        Type,
        types::{FixedArrayType, FunctionType, MutType, PointerType, SliceType, SymbolType},
    },
    lexer::token::TokenKind::{self, self as T},
    parser::{
        Parser,
        lookups::{
            BindingPower::{self, self as BP},
            BpLookup,
        },
    },
};

type TypeNudHandler = fn(&mut Parser) -> Result<Type>;
type TypeLedHandler = fn(&mut Parser, Type, BindingPower) -> Result<Type>;

type TypeNudLookup = HashMap<TokenKind, TypeNudHandler>;
type TypeLedLookup = HashMap<TokenKind, TypeLedHandler>;

static INITIALIZE: Once = Once::new();
pub static TYPE_BP_LU: OnceLock<BpLookup> = OnceLock::new();
pub static TYPE_NUD_LU: OnceLock<TypeNudLookup> = OnceLock::new();
pub static TYPE_LED_LU: OnceLock<TypeLedLookup> = OnceLock::new();

#[allow(dead_code)]
fn type_led(
    kind: TokenKind,
    bp: BindingPower,
    led_fn: TypeLedHandler,
    bp_lu: &mut BpLookup,
    led_lu: &mut TypeLedLookup,
) {
    bp_lu.insert(kind, bp);
    led_lu.insert(kind, led_fn);
}

fn type_nud(kind: TokenKind, nud_fn: TypeNudHandler, nud_lu: &mut TypeNudLookup) {
    nud_lu.insert(kind, nud_fn);
}

pub fn create_token_type_lookups() {
    INITIALIZE.call_once(|| {
        let bp_lu = BpLookup::new();
        let mut nud_lu = TypeNudLookup::new();
        let led_lu = TypeLedLookup::new();

        type_nud(T::Identifier, parse_symbol_type, &mut nud_lu);
        type_nud(T::OpenBracket, parse_array_type, &mut nud_lu);
        type_nud(T::Mut, parse_mut_type, &mut nud_lu);
        type_nud(T::OpenParen, parse_function_type, &mut nud_lu);
        type_nud(T::Reference, parse_pointer_type, &mut nud_lu);

        let _ = TYPE_BP_LU.set(bp_lu);
        let _ = TYPE_NUD_LU.set(nud_lu);
        let _ = TYPE_LED_LU.set(led_lu);
    });
}

fn parse_symbol_type(parser: &mut Parser) -> Result<Type> {
    Ok(Type::Symbol(SymbolType {
        name: parser.expect(T::Identifier)?.value,
    }))
}

fn parse_pointer_type(parser: &mut Parser) -> Result<Type> {
    parser.expect(T::Reference)?;
    let underlying = parse_type(parser, BindingPower::DefaultBp)?;
    Ok(Type::Pointer(PointerType {
        underlying: Box::new(underlying),
    }))
}

fn parse_array_type(parser: &mut Parser) -> Result<Type> {
    parser.advance();

    match parser.current_token().kind {
        T::Number => {
            let length = parser.current_token().value.parse::<usize>()?;
            parser.advance();
            parser.expect(T::CloseBracket)?;
            let underlying = parse_type(parser, BP::DefaultBp)?;
            Ok(Type::FixedArray(FixedArrayType {
                length,
                underlying: Box::new(underlying),
            }))
        }
        T::CloseBracket => {
            parser.advance();
            let underlying = parse_type(parser, BP::DefaultBp)?;
            Ok(Type::Slice(SliceType {
                underlying: Box::new(underlying),
            }))
        }
        _ => Err(anyhow!(
            format!(
                "Expected number or ']' in array type, got {:?}",
                parser.current_token()
            )
            .red()
            .bold()
        )),
    }
}

pub fn parse_type(parser: &mut Parser, bp: BindingPower) -> Result<Type> {
    let token_kind = parser.current_token().kind;

    let bp_lu = TYPE_BP_LU.get().expect("Type lookups not initialized");
    let nud_lu = TYPE_NUD_LU.get().expect("Type lookups not initialized");
    let led_lu = TYPE_LED_LU.get().expect("Type lookups not initialized");

    let nud_fn = {
        nud_lu.get(&token_kind).cloned().ok_or_else(|| {
            anyhow!(
                format!("Type nud handler expected for token {token_kind:?}")
                    .red()
                    .bold()
            )
        })?
    };

    let mut left = nud_fn(parser)?;

    loop {
        let current_bp = {
            *bp_lu
                .get(&parser.current_token().kind)
                .unwrap_or(&BindingPower::DefaultBp)
        };

        if current_bp <= bp {
            break;
        }

        let token_kind = parser.current_token().kind;
        let led_fn = {
            led_lu.get(&token_kind).cloned().ok_or_else(|| {
                anyhow!(
                    format!("Type led handler expected for token {token_kind:?}")
                        .red()
                        .bold()
                )
            })?
        };

        left = led_fn(parser, left, current_bp)?;
    }

    Ok(left)
}

fn parse_mut_type(parser: &mut Parser) -> Result<Type> {
    parser.expect(TokenKind::Mut)?;
    let underlying = parse_type(parser, BindingPower::DefaultBp)?;
    Ok(Type::Mut(MutType {
        underlying: Box::new(underlying),
    }))
}

fn parse_function_type(parser: &mut Parser) -> Result<Type> {
    parser.expect(TokenKind::OpenParen)?;

    let mut parameters = Vec::new();
    while parser.current_token().kind != TokenKind::CloseParen {
        parameters.push(parse_type(parser, BindingPower::DefaultBp)?);

        if parser.current_token().kind == TokenKind::Comma {
            parser.advance();
        } else if parser.current_token().kind != TokenKind::CloseParen {
            bail!(
                "Expected comma or closing parenthesis in function type"
                    .red()
                    .bold()
            );
        }
    }
    parser.expect(TokenKind::CloseParen)?;

    parser.expect(TokenKind::Arrow)?;
    let return_type = parse_type(parser, BindingPower::DefaultBp)?;

    Ok(Type::Function(FunctionType {
        parameters,
        return_type: Box::new(return_type),
    }))
}
