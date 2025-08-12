use std::{collections::HashMap, sync::Mutex};

use lazy_static::lazy_static;

use crate::{
    ast::ast::{Expression, Statement},
    lexer::token::TokenKind::{self, self as TK},
    parser::{
        expr::{parse_function_call_expr, parse_member_access_expr},
        stmt::{parse_pub_stmt, parse_return_stmt},
    },
};

use super::{
    expr::{
        parse_array_literal_expr, parse_assignment_expr, parse_binary_expr, parse_grouping_expr,
        parse_prefix_expr, parse_primary_expr, parse_struct_instantiation_expr,
    },
    parser::Parser,
    stmt::{parse_fn_decl_stmt, parse_struct_decl_stmt, parse_var_decl_statement},
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
use BindingPower as BP;

type StmtHandler = fn(&mut Parser) -> anyhow::Result<Statement>;
type NudHandler = fn(&mut Parser) -> anyhow::Result<Expression>;
type LedHandler = fn(&mut Parser, Expression, BindingPower) -> anyhow::Result<Expression>;

type StmtLookup = HashMap<TokenKind, StmtHandler>;
type NudLookup = HashMap<TokenKind, NudHandler>;
type LedLookup = HashMap<TokenKind, LedHandler>;
pub type BpLookup = HashMap<TokenKind, BindingPower>;

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

fn nud(kind: TokenKind, nud_fn: NudHandler) {
    NUD_LU.lock().unwrap().insert(kind, nud_fn);
}

fn stmt(kind: TokenKind, stmt_fn: StmtHandler) {
    BP_LU.lock().unwrap().insert(kind, BindingPower::DefaultBp);
    STMT_LU.lock().unwrap().insert(kind, stmt_fn);
}

pub fn create_token_lookups() {
    // Assignment
    led(TK::Equals, BP::Assignment, parse_assignment_expr);
    led(TK::PlusEquals, BP::Assignment, parse_assignment_expr);
    led(TK::MinusEquals, BP::Assignment, parse_assignment_expr);
    led(TK::StarEquals, BP::Assignment, parse_assignment_expr);
    led(TK::SlashEquals, BP::Assignment, parse_assignment_expr);
    led(TK::PercentEquals, BP::Assignment, parse_assignment_expr);

    // Logical
    led(TK::And, BP::Logical, parse_binary_expr);
    led(TK::Or, BP::Logical, parse_binary_expr);
    led(TK::DotDot, BP::Logical, parse_binary_expr);

    // Relational
    led(TK::Less, BP::Relational, parse_binary_expr);
    led(TK::More, BP::Relational, parse_binary_expr);
    led(TK::LessEquals, BP::Relational, parse_binary_expr);
    led(TK::MoreEquals, BP::Relational, parse_binary_expr);
    led(TK::EqualsEquals, BP::Relational, parse_binary_expr);
    led(TK::NotEquals, BP::Relational, parse_binary_expr);

    // Additive
    led(TK::Plus, BP::Additive, parse_binary_expr);
    led(TK::Dash, BP::Additive, parse_binary_expr);

    // MUltiplicative
    led(TK::Star, BP::Multiplicative, parse_binary_expr);
    led(TK::Slash, BP::Multiplicative, parse_binary_expr);
    led(TK::Percent, BP::Multiplicative, parse_binary_expr);

    // Literals & Symbols
    nud(TK::Number, parse_primary_expr);
    nud(TK::StringLiteral, parse_primary_expr);
    nud(TK::Identifier, parse_primary_expr);
    nud(TK::OpenParen, parse_grouping_expr);
    nud(TK::Dash, parse_prefix_expr);
    nud(TK::Reference, parse_prefix_expr);

    // Call & Member
    led(TK::OpenCurly, BP::Call, parse_struct_instantiation_expr);
    led(TK::OpenParen, BP::Call, parse_function_call_expr);
    led(TK::Dot, BP::Member, parse_member_access_expr);
    nud(TK::OpenBracket, parse_array_literal_expr);

    // Statements
    stmt(TK::Let, parse_var_decl_statement);
    stmt(TK::Static, parse_var_decl_statement);

    stmt(TK::Struct, parse_struct_decl_stmt);
    stmt(TK::Fn, parse_fn_decl_stmt);
    stmt(TK::Return, parse_return_stmt);

    stmt(TK::Pub, parse_pub_stmt);
}
