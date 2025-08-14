use anyhow::{anyhow, bail, Result};
use std::{
    collections::HashMap,
    mem::{self, Discriminant},
    sync::Mutex,
};

use colored::Colorize;
use lazy_static::lazy_static;

use crate::{
    ast::{
        ast::Type,
        types::{ConstType, FixedArrayType, FunctionType, SliceType, SymbolType},
    },
    lexer::token::Token::{self, self as T},
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

type TypeNudLookup = HashMap<Discriminant<Token>, TypeNudHandler>;
type TypeLedLookup = HashMap<Discriminant<Token>, TypeLedHandler>;

lazy_static! {
    pub static ref TYPE_BP_LU: Mutex<BpLookup> = Mutex::new(HashMap::new());
    pub static ref TYPE_NUD_LU: Mutex<TypeNudLookup> = Mutex::new(HashMap::new());
    pub static ref TYPE_LED_LU: Mutex<TypeLedLookup> = Mutex::new(HashMap::new());
}

fn type_led(kind: Token, bp: BindingPower, led_fn: TypeLedHandler) {
    TYPE_BP_LU
        .lock()
        .unwrap()
        .insert(mem::discriminant(&kind), bp);
    TYPE_LED_LU
        .lock()
        .unwrap()
        .insert(mem::discriminant(&kind), led_fn);
}

fn type_nud(kind: Token, nud_fn: TypeNudHandler) {
    TYPE_NUD_LU
        .lock()
        .unwrap()
        .insert(mem::discriminant(&kind), nud_fn);
}

pub fn create_token_type_lookups() {
    type_nud(T::Identifier("".to_string()), parse_symbol_type);
    type_nud(T::OpenBracket, parse_array_type);
    type_nud(T::Const, parse_const_type);
    type_nud(T::OpenParen, parse_function_type);
}

fn parse_symbol_type(parser: &mut Parser) -> Result<Type> {
    Ok(Type::Symbol(SymbolType {
        name: parser.expect(T::identifier())?.unwrap_identifier(),
    }))
}

fn parse_array_type(parser: &mut Parser) -> Result<Type> {
    parser.advance();

    match parser.current_token() {
        T::Number(length) => {
            parser.advance();
            parser.expect(T::CloseBracket)?;
            let underlying = parse_type(parser, BP::DefaultBp)?;
            Ok(Type::FixedArray(FixedArrayType {
                length: length as usize,
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
        _ => Err(anyhow!(format!(
            "Expected number or ']' in array type, got {:?}",
            parser.current_token()
        )
        .red()
        .bold())),
    }
}

pub fn parse_type(parser: &mut Parser, bp: BindingPower) -> Result<Type> {
    let token_kind = parser.current_token();

    let nud_fn = {
        let nud_lu = TYPE_NUD_LU.lock().unwrap();
        nud_lu
            .get(&mem::discriminant(&token_kind))
            .cloned()
            .ok_or_else(|| {
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
                .get(&mem::discriminant(&parser.current_token()))
                .unwrap_or(&BindingPower::DefaultBp)
        };

        if current_bp <= bp {
            break;
        }

        let token_kind = parser.current_token();
        let led_fn = {
            let led_lu = TYPE_LED_LU.lock().unwrap();
            led_lu
                .get(&mem::discriminant(&token_kind))
                .cloned()
                .ok_or_else(|| {
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
    parser.expect(Token::Const)?;
    let underlying = parse_type(parser, BindingPower::DefaultBp)?;
    Ok(Type::Const(ConstType {
        underlying: Box::new(underlying),
    }))
}

fn parse_function_type(parser: &mut Parser) -> Result<Type> {
    parser.expect(Token::OpenParen)?;

    let mut parameters = Vec::new();
    while parser.current_token() != Token::CloseParen {
        parameters.push(parse_type(parser, BindingPower::DefaultBp)?);

        if parser.current_token() == Token::Comma {
            parser.advance();
        } else if parser.current_token() != Token::CloseParen {
            bail!("Expected comma or closing parenthesis in function type"
                .red()
                .bold());
        }
    }
    parser.expect(Token::CloseParen)?;

    parser.expect(Token::Arrow)?;
    let return_type = parse_type(parser, BindingPower::DefaultBp)?;

    Ok(Type::Function(FunctionType {
        parameters,
        return_type: Box::new(return_type),
    }))
}
