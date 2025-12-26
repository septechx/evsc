use std::collections::HashMap;

use lazy_static::lazy_static;
use parking_lot::Mutex;

use crate::{
    ast::ast::{Expression, Statement},
    lexer::token::TokenKind::{self, self as T},
    parser::{
        expr::{
            parse_array_literal_expr, parse_assignment_expr, parse_binary_expr,
            parse_function_call_expr, parse_grouping_expr, parse_member_access_expr,
            parse_prefix_expr, parse_primary_expr, parse_struct_instantiation_expr,
            parse_type_expr,
        },
        parser::Parser,
        stmt::{
            parse_fn_decl_stmt, parse_pub_stmt, parse_return_stmt, parse_struct_decl_stmt,
            parse_var_decl_statement,
        },
    },
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
    BP_LU.lock().insert(kind, bp);
    LED_LU.lock().insert(kind, led_fn);
}

fn nud(kind: TokenKind, nud_fn: NudHandler) {
    NUD_LU.lock().insert(kind, nud_fn);
}

fn stmt(kind: TokenKind, stmt_fn: StmtHandler) {
    BP_LU.lock().insert(kind, BindingPower::DefaultBp);
    STMT_LU.lock().insert(kind, stmt_fn);
}

pub fn create_token_lookups() {
    // Assignment
    led(T::Equals, BP::Assignment, parse_assignment_expr);
    led(T::PlusEquals, BP::Assignment, parse_assignment_expr);
    led(T::MinusEquals, BP::Assignment, parse_assignment_expr);
    led(T::StarEquals, BP::Assignment, parse_assignment_expr);
    led(T::SlashEquals, BP::Assignment, parse_assignment_expr);
    led(T::PercentEquals, BP::Assignment, parse_assignment_expr);

    // Logical
    led(T::And, BP::Logical, parse_binary_expr);
    led(T::Or, BP::Logical, parse_binary_expr);
    led(T::DotDot, BP::Logical, parse_binary_expr);

    // Relational
    led(T::Less, BP::Relational, parse_binary_expr);
    led(T::More, BP::Relational, parse_binary_expr);
    led(T::LessEquals, BP::Relational, parse_binary_expr);
    led(T::MoreEquals, BP::Relational, parse_binary_expr);
    led(T::EqualsEquals, BP::Relational, parse_binary_expr);
    led(T::NotEquals, BP::Relational, parse_binary_expr);

    // Additive
    led(T::Plus, BP::Additive, parse_binary_expr);
    led(T::Dash, BP::Additive, parse_binary_expr);

    // Multiplicative
    led(T::Star, BP::Multiplicative, parse_binary_expr);
    led(T::Slash, BP::Multiplicative, parse_binary_expr);
    led(T::Percent, BP::Multiplicative, parse_binary_expr);

    // Literals & Symbols
    nud(T::Number, parse_primary_expr);
    nud(T::StringLiteral, parse_primary_expr);
    nud(T::Identifier, parse_primary_expr);
    nud(T::OpenParen, parse_grouping_expr);
    nud(T::Dash, parse_prefix_expr);
    nud(T::Reference, parse_prefix_expr);
    nud(T::Dollar, parse_type_expr);

    // Call & Member
    led(T::OpenCurly, BP::Call, parse_struct_instantiation_expr);
    led(T::OpenParen, BP::Call, parse_function_call_expr);
    led(T::Dot, BP::Member, parse_member_access_expr);
    nud(T::OpenBracket, parse_array_literal_expr);

    // Statements
    stmt(T::Let, parse_var_decl_statement);
    stmt(T::Static, parse_var_decl_statement);

    stmt(T::Struct, parse_struct_decl_stmt);
    stmt(T::Fn, parse_fn_decl_stmt);
    stmt(T::Return, parse_return_stmt);

    stmt(T::Pub, parse_pub_stmt);
}
