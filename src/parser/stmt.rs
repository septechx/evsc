use crate::{
    ast::{ast::Stmt, statements::ExpressionStmt},
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
