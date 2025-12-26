use anyhow::{Result, anyhow, bail};
use std::{collections::HashMap, sync::Mutex};

use colored::Colorize;
use lazy_static::lazy_static;

use crate::{
    ast::{
        Type,
        types::{ConstType, FixedArrayType, FunctionType, SliceType, SymbolType},
    },
    lexer::token::TokenKind::{self, self as T},
    parser::{
        lookups::{
            BindingPower::{self, self as BP},
            BpLookup,
        },
        parser::Parser,
    },
};

type TypeNudHandler = fn(&mut Parser) -> Result<Type>;
type TypeLedHandler = fn(&mut Parser, Type, BindingPower) -> Result<Type>;

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
    type_nud(T::Identifier, parse_symbol_type);
    type_nud(T::OpenBracket, parse_array_type);
    type_nud(T::Const, parse_const_type);
    type_nud(T::OpenParen, parse_function_type);
}

pub fn parse_symbol_type(parser: &mut Parser) -> Result<Type> {
    Ok(Type::Symbol(SymbolType {
        name: parser.expect(T::Identifier)?.value,
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

    let nud_fn = {
        let nud_lu = TYPE_NUD_LU.lock().unwrap();
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
            let bp_lu = TYPE_BP_LU.lock().unwrap();
            *bp_lu
                .get(&parser.current_token().kind)
                .unwrap_or(&BindingPower::DefaultBp)
        };

        if current_bp <= bp {
            break;
        }

        let token_kind = parser.current_token().kind;
        let led_fn = {
            let led_lu = TYPE_LED_LU.lock().unwrap();
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

fn parse_const_type(parser: &mut Parser) -> Result<Type> {
    parser.expect(TokenKind::Const)?;
    let underlying = parse_type(parser, BindingPower::DefaultBp)?;
    Ok(Type::Const(ConstType {
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
