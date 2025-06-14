use std::{collections::HashMap, sync::Mutex};

use lazy_static::lazy_static;

use crate::{
    ast::{
        ast::{Expr, Stmt},
        expressions::{NumberExpr, StringExpr, SymbolExpr},
    },
    lexer::token::{
        Token,
        TokenKind::{self, *},
    },
};

use super::{
    expr::{parse_binary_expr, parse_primary_expr},
    parser::Parser,
    stmt::parser_var_decl_statement,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum BindingPower {
    DefaultBp,
    Comma,
    Assignment,
    Logical,
    Relational,
    Additive,
    Multiplicative,
    Unary,
    Call,
    Member,
    Primary,
}

type StmtHandler = fn(&mut Parser) -> Box<dyn Stmt>;
type NudHandler = fn(&mut Parser) -> Box<dyn Expr>;
type LedHandler = fn(&mut Parser, Box<dyn Expr>, BindingPower) -> Box<dyn Expr>;

type StmtLookup = HashMap<TokenKind, StmtHandler>;
type NudLookup = HashMap<TokenKind, NudHandler>;
type LedLookup = HashMap<TokenKind, LedHandler>;
type BpLookup = HashMap<TokenKind, BindingPower>;

lazy_static! {
    pub static ref BP_LU: Mutex<BpLookup> = Mutex::new(HashMap::new());
    pub static ref NUD_LU: Mutex<NudLookup> = Mutex::new(HashMap::new());
    pub static ref LED_LU: Mutex<LedLookup> = Mutex::new(HashMap::new());
    pub static ref STMT_LU: Mutex<StmtLookup> = Mutex::new(HashMap::new());
}

fn led(kind: TokenKind, bp: BindingPower, led_fn: LedHandler) {
    BP_LU.lock().unwrap().insert(kind, bp);
    LED_LU.lock().unwrap().insert(kind, led_fn);
}

fn nud(kind: TokenKind, bp: BindingPower, nud_fn: NudHandler) {
    BP_LU.lock().unwrap().insert(kind, BindingPower::Primary);
    NUD_LU.lock().unwrap().insert(kind, nud_fn);
}

fn stmt(kind: TokenKind, stmt_fn: StmtHandler) {
    BP_LU.lock().unwrap().insert(kind, BindingPower::DefaultBp);
    STMT_LU.lock().unwrap().insert(kind, stmt_fn);
}

use BindingPower::*;

pub fn create_token_lookups() {
    // Logical
    led(And, Logical, parse_binary_expr);
    led(Or, Logical, parse_binary_expr);
    led(DotDot, Logical, parse_binary_expr);

    // Relational
    led(Less, Relational, parse_binary_expr);
    led(More, Relational, parse_binary_expr);
    led(LessEquals, Relational, parse_binary_expr);
    led(MoreEquals, Relational, parse_binary_expr);
    led(EqualsEquals, Relational, parse_binary_expr);
    led(NotEquals, Relational, parse_binary_expr);

    // Additive
    led(Plus, Additive, parse_binary_expr);
    led(Dash, Additive, parse_binary_expr);

    // MUltiplicative
    led(Star, Multiplicative, parse_binary_expr);
    led(Slash, Multiplicative, parse_binary_expr);
    led(Percent, Multiplicative, parse_binary_expr);

    // Literals & Symbols
    nud(Number, Primary, parse_primary_expr);
    nud(StringLiteral, Primary, parse_primary_expr);
    nud(Identifier, Primary, parse_primary_expr);

    // Statements
    stmt(Const, parser_var_decl_statement);
    stmt(Let, parser_var_decl_statement);
}
