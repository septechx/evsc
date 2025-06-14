use std::collections::HashMap;

use colored::Colorize;

use crate::{
    ast::{
        ast::{Expr, Stmt, Type},
        statements::{ExpressionStmt, StructDeclStmt, StructMethod, StructProperty, VarDeclStmt},
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

pub fn parse_var_decl_statement(parser: &mut Parser) -> Box<dyn Stmt> {
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

pub fn parse_struct_decl_stmt(parser: &mut Parser) -> Box<dyn Stmt> {
    parser.expect(TokenKind::Struct);
    let mut properties: HashMap<String, StructProperty> = HashMap::new();
    let mut methods: HashMap<String, StructMethod> = HashMap::new();
    let name = parser.expect(TokenKind::Identifier).value;

    parser.expect(TokenKind::OpenCurly);

    loop {
        if !parser.has_tokens() || parser.current_token_kind() == TokenKind::CloseCurly {
            break;
        }

        let mut is_static: bool = false;
        let mut property_name: String;

        if parser.current_token_kind() == TokenKind::Static {
            is_static = true;
            parser.expect(TokenKind::Static);
        }

        if parser.current_token_kind() == TokenKind::Identifier {
            property_name = parser.expect(TokenKind::Identifier).value;
            parser.expect_error(
                TokenKind::Colon,
                Some(String::from(
                    "Expected colon after property name in struct property declaration",
                )),
            );
            let explicit_type = parse_type(parser, BindingPower::DefaultBp);

            if parser.current_token_kind() != TokenKind::CloseCurly {
                parser.expect(TokenKind::Comma);
            }

            if properties.contains_key(&property_name) {
                panic!(
                    "{}",
                    format!(
                        "Property {} has already been defined in struct",
                        property_name
                    )
                    .red()
                    .bold()
                );
            }

            properties.insert(
                property_name,
                StructProperty {
                    is_static,
                    explicit_type,
                },
            );

            continue;
        }

        todo!("Handle methods")
    }

    parser.expect(TokenKind::CloseCurly);

    Box::new(StructDeclStmt {
        name,
        properties,
        methods,
    })
}
