use anyhow::{Result, anyhow, bail};
use parking_lot::Once;
use std::{collections::HashMap, sync::OnceLock};

use colored::Colorize;

use crate::{
    ast::{
        Type, TypeKind,
        types::{
            FixedArrayType, FunctionType, MutType, PointerType, SliceType, SymbolType, TupleType,
        },
    },
    lexer::token::TokenKind::{self, self as T},
    parser::{
        Parser,
        lookups::{
            BindingPower::{self, self as BP},
            BpLookup,
        },
    },
    span::Span,
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
        type_nud(T::OpenParen, parse_parenthesis_type, &mut nud_lu);
        type_nud(T::Reference, parse_pointer_type, &mut nud_lu);

        let _ = TYPE_BP_LU.set(bp_lu);
        let _ = TYPE_NUD_LU.set(nud_lu);
        let _ = TYPE_LED_LU.set(led_lu);
    });
}

fn parse_symbol_type(parser: &mut Parser) -> Result<Type> {
    let ident = parser.expect_identifier()?;
    let span = ident.span;

    Ok(parser.type_(TypeKind::Symbol(SymbolType { name: ident }), span))
}

fn parse_pointer_type(parser: &mut Parser) -> Result<Type> {
    let start_token = parser.expect(T::Reference)?;
    let underlying = parse_type(parser, BindingPower::DefaultBp)?;
    let end_span = underlying.span;

    Ok(parser.type_(
        TypeKind::Pointer(PointerType {
            underlying: Box::new(underlying),
        }),
        Span::new(start_token.span.start(), end_span.end()),
    ))
}

fn parse_array_type(parser: &mut Parser) -> Result<Type> {
    let start_token = parser.advance();

    match parser.current_token().kind {
        T::Number => {
            let length = parser.current_token().value.parse::<usize>()?;
            parser.advance();
            parser.expect(T::CloseBracket)?;
            let underlying = parse_type(parser, BP::DefaultBp)?;
            let end_span = underlying.span;

            Ok(parser.type_(
                TypeKind::FixedArray(FixedArrayType {
                    length,
                    underlying: Box::new(underlying),
                }),
                Span::new(start_token.span.start(), end_span.end()),
            ))
        }
        T::CloseBracket => {
            parser.advance();
            let underlying = parse_type(parser, BP::DefaultBp)?;
            let end_span = underlying.span;

            Ok(parser.type_(
                TypeKind::Slice(SliceType {
                    underlying: Box::new(underlying),
                }),
                Span::new(start_token.span.start(), end_span.end()),
            ))
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
    let start_token = parser.expect(TokenKind::Mut)?;
    let underlying = parse_type(parser, BindingPower::DefaultBp)?;
    let end_span = underlying.span;

    Ok(parser.type_(
        TypeKind::Mut(MutType {
            underlying: Box::new(underlying),
        }),
        Span::new(start_token.span.start(), end_span.end()),
    ))
}

fn parse_parenthesis_type(parser: &mut Parser) -> Result<Type> {
    let start_token = parser.current_token();

    let mut types = Vec::new();
    let mut has_comma = false;
    parser.advance();

    while parser.current_token().kind != TokenKind::CloseParen {
        types.push(parse_type(parser, BindingPower::DefaultBp)?);

        if parser.current_token().kind == TokenKind::Comma {
            has_comma = true;
            parser.advance();
        } else if parser.current_token().kind != TokenKind::CloseParen {
            bail!("Expected comma or closing parenthesis in type".red().bold());
        }
    }
    let close_token = parser.expect(TokenKind::CloseParen)?;

    if parser.current_token().kind == TokenKind::Arrow {
        parser.expect(TokenKind::Arrow)?;
        let return_type = parse_type(parser, BindingPower::DefaultBp)?;
        let end_span = return_type.span;

        Ok(parser.type_(
            TypeKind::Function(FunctionType {
                parameters: types.into_boxed_slice(),
                return_type: Box::new(return_type),
            }),
            Span::new(start_token.span.start(), end_span.end()),
        ))
    } else if types.len() == 1 {
        if has_comma {
            Ok(parser.type_(
                TypeKind::Tuple(TupleType {
                    elements: types.into_boxed_slice(),
                }),
                Span::new(start_token.span.start(), close_token.span.end()),
            ))
        } else {
            Ok(types[0].clone())
        }
    } else {
        Ok(parser.type_(
            TypeKind::Tuple(TupleType {
                elements: types.into_boxed_slice(),
            }),
            Span::new(start_token.span.start(), close_token.span.end()),
        ))
    }
}
