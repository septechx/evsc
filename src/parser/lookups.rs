use std::{collections::HashMap, sync::OnceLock};

use parking_lot::Once;

use crate::{
    ast::{Attribute, Expr, Stmt},
    lexer::token::TokenKind::{self, self as T},
    parser::{Parser, expr::*, stmt::*},
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

type StmtHandler = fn(&mut Parser, Vec<Attribute>) -> anyhow::Result<Stmt>;
type NudHandler = fn(&mut Parser) -> anyhow::Result<Expr>;
type LedHandler = fn(&mut Parser, Expr, BindingPower) -> anyhow::Result<Expr>;

type StmtLookup = HashMap<TokenKind, StmtHandler>;
type NudLookup = HashMap<TokenKind, NudHandler>;
type LedLookup = HashMap<TokenKind, LedHandler>;
pub type BpLookup = HashMap<TokenKind, BindingPower>;

static INITIALIZE: Once = Once::new();
pub static BP_LU: OnceLock<BpLookup> = OnceLock::new();
pub static NUD_LU: OnceLock<NudLookup> = OnceLock::new();
pub static LED_LU: OnceLock<LedLookup> = OnceLock::new();
pub static STMT_LU: OnceLock<StmtLookup> = OnceLock::new();

fn led(
    kind: TokenKind,
    bp: BindingPower,
    led_fn: LedHandler,
    bp_lu: &mut BpLookup,
    led_lu: &mut LedLookup,
) {
    bp_lu.insert(kind, bp);
    led_lu.insert(kind, led_fn);
}

fn nud(kind: TokenKind, nud_fn: NudHandler, nud_lu: &mut NudLookup) {
    nud_lu.insert(kind, nud_fn);
}

fn stmt(kind: TokenKind, stmt_fn: StmtHandler, bp_lu: &mut BpLookup, stmt_lu: &mut StmtLookup) {
    bp_lu.insert(kind, BindingPower::DefaultBp);
    stmt_lu.insert(kind, stmt_fn);
}

pub fn create_token_lookups() {
    INITIALIZE.call_once(|| {
        let mut bp_lu = BpLookup::new();
        let mut nud_lu = NudLookup::new();
        let mut led_lu = LedLookup::new();
        let mut stmt_lu = StmtLookup::new();

        // Assignment
        led(
            T::Equals,
            BP::Assignment,
            parse_assignment_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::PlusEquals,
            BP::Assignment,
            parse_assignment_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::MinusEquals,
            BP::Assignment,
            parse_assignment_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::StarEquals,
            BP::Assignment,
            parse_assignment_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::SlashEquals,
            BP::Assignment,
            parse_assignment_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::PercentEquals,
            BP::Assignment,
            parse_assignment_expr,
            &mut bp_lu,
            &mut led_lu,
        );

        // Logical
        led(
            T::And,
            BP::Logical,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::Or,
            BP::Logical,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::DotDot,
            BP::Logical,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );

        // Relational
        led(
            T::Less,
            BP::Relational,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::More,
            BP::Relational,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::LessEquals,
            BP::Relational,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::MoreEquals,
            BP::Relational,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::EqualsEquals,
            BP::Relational,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::NotEquals,
            BP::Relational,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );

        // Additive
        led(
            T::Plus,
            BP::Additive,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::Dash,
            BP::Additive,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );

        // Multiplicative
        led(
            T::Star,
            BP::Multiplicative,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::Slash,
            BP::Multiplicative,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::Percent,
            BP::Multiplicative,
            parse_binary_expr,
            &mut bp_lu,
            &mut led_lu,
        );

        // Literals & Symbols
        nud(T::Number, parse_primary_expr, &mut nud_lu);
        nud(T::StringLiteral, parse_primary_expr, &mut nud_lu);
        nud(T::Identifier, parse_primary_expr, &mut nud_lu);
        nud(T::OpenParen, parse_grouping_expr, &mut nud_lu);
        nud(T::Dash, parse_prefix_expr, &mut nud_lu);
        nud(T::Reference, parse_prefix_expr, &mut nud_lu);
        nud(T::Dollar, parse_type_expr, &mut nud_lu);

        // Call & Member
        led(
            T::OpenCurly,
            BP::Call,
            parse_struct_instantiation_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::OpenParen,
            BP::Call,
            parse_function_call_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        led(
            T::Dot,
            BP::Member,
            parse_member_access_expr,
            &mut bp_lu,
            &mut led_lu,
        );
        nud(T::OpenBracket, parse_array_literal_expr, &mut nud_lu);

        // Statements
        stmt(T::Let, parse_var_decl_statement, &mut bp_lu, &mut stmt_lu);
        stmt(
            T::Static,
            parse_var_decl_statement,
            &mut bp_lu,
            &mut stmt_lu,
        );

        stmt(T::Struct, parse_struct_decl_stmt, &mut bp_lu, &mut stmt_lu);
        stmt(
            T::Interface,
            parse_interface_decl_stmt,
            &mut bp_lu,
            &mut stmt_lu,
        );
        stmt(T::Fn, parse_fn_decl_stmt, &mut bp_lu, &mut stmt_lu);
        stmt(T::Return, parse_return_stmt, &mut bp_lu, &mut stmt_lu);

        stmt(T::Pub, parse_pub_stmt, &mut bp_lu, &mut stmt_lu);

        let _ = BP_LU.set(bp_lu);
        let _ = NUD_LU.set(nud_lu);
        let _ = LED_LU.set(led_lu);
        let _ = STMT_LU.set(stmt_lu);
    });
}
