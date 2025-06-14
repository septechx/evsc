use colored::Colorize;

use crate::{
    ast::{
        ast::{Expr, Stmt, Type},
        statements::{ExpressionStmt, VarDeclStmt},
    },
    lexer::token::TokenKind,
};

use super::{
    expr::parse_expr,
    lookups::{BindingPower, STMT_LU},
    parser::Parser,
    types::parse_type,
};

pub fn parse_stmt(parser: &mut Parser) -> Box<dyn Stmt> {
    let stmt_lu = STMT_LU.lock().unwrap();
    let stmt_fn = stmt_lu.get(&parser.current_token_kind());

    if let Some(stmt_fn) = stmt_fn {
        stmt_fn(parser)
    } else {
        let expression = parse_expr(parser, BindingPower::DefaultBp);
        parser.expect(TokenKind::Semicolon);

        Box::new(ExpressionStmt { expression })
    }
}

pub fn parser_var_decl_statement(parser: &mut Parser) -> Box<dyn Stmt> {
    let mut explicit_type: Option<Box<dyn Type>> = None;
    let mut assigned_value: Option<Box<dyn Expr>> = None;

    let is_constant = parser.advance().kind == TokenKind::Const;
    let variable_name = parser
        .expect_error(
            TokenKind::Identifier,
            Some(String::from(
                "Expected variable name inside variable declaration",
            )),
        )
        .value;

    if parser.current_token_kind() == TokenKind::Colon {
        parser.advance();
        explicit_type = Some(parse_type(parser, BindingPower::DefaultBp));
    }

    if parser.current_token_kind() != TokenKind::Semicolon {
        parser.expect(TokenKind::Equals);
        assigned_value = Some(parse_expr(parser, BindingPower::Assignment));
    } else if explicit_type.is_none() {
        panic!(
            "{}",
            format!("Missing type or value in variable declaration")
                .red()
                .bold()
        );
    }

    parser.expect(TokenKind::Semicolon);

    if is_constant && assigned_value.is_none() {
        panic!(
            "{}",
            format!("Cannot define constant without providing a value")
                .red()
                .bold()
        );
    }

    Box::new(VarDeclStmt {
        explicit_type,
        is_constant,
        variable_name,
        assigned_value,
    })
}
