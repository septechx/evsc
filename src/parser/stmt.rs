use crate::{
    ast::{
        ast::Stmt,
        statements::{ExpressionStmt, VarDeclStmt},
    },
    lexer::token::TokenKind,
};

use super::{
    expr::parse_expr,
    lookups::{BindingPower, STMT_LU},
    parser::Parser,
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
    let is_constant = parser.advance().kind == TokenKind::Const;
    let variable_name = parser
        .expect_error(
            TokenKind::Identifier,
            Some(String::from(
                "Expected variable name inside variable declaration",
            )),
        )
        .value;
    parser.expect(TokenKind::Equals);
    let assigned_value = parse_expr(parser, BindingPower::Assignment);
    parser.expect(TokenKind::Semicolon);

    Box::new(VarDeclStmt {
        is_constant,
        variable_name,
        assigned_value,
    })
}
