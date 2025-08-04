use colored::Colorize;

use crate::{
    ast::{
        ast::{Expression, Statement, Type},
        statements::{
            ExpressionStmt, FnArgument, FnDeclStmt, ReturnStmt, StructDeclStmt, StructMethod,
            StructProperty, VarDeclStmt,
        },
    },
    lexer::token::TokenKind,
};

use super::{
    expr::parse_expr,
    lookups::{BindingPower, STMT_LU},
    parser::Parser,
    types::parse_type,
};

pub fn parse_stmt(parser: &mut Parser) -> anyhow::Result<Statement> {
    let stmt_fn = {
        let stmt_lu = STMT_LU.lock().unwrap();
        stmt_lu.get(&parser.current_token_kind()).cloned()
    };

    if let Some(stmt_fn) = stmt_fn {
        stmt_fn(parser)
    } else {
        let expression = parse_expr(parser, BindingPower::DefaultBp)?;
        parser.expect(TokenKind::Semicolon)?;

        Ok(Statement::Expression(ExpressionStmt { expression }))
    }
}

pub fn parse_var_decl_statement(parser: &mut Parser) -> anyhow::Result<Statement> {
    let mut explicit_type: Option<Type> = None;
    let mut assigned_value: Option<Expression> = None;

    // TODO: Implement checking for mutability qualifier
    let is_constant = true;
    let variable_name = parser
        .expect_error(
            TokenKind::Identifier,
            Some(String::from(
                "Expected variable name inside variable declaration",
            )),
        )?
        .value;

    if parser.current_token_kind() == TokenKind::Colon {
        parser.advance();
        explicit_type = Some(parse_type(parser, BindingPower::DefaultBp)?);
    }

    if parser.current_token_kind() != TokenKind::Semicolon {
        parser.expect(TokenKind::Equals)?;
        assigned_value = Some(parse_expr(parser, BindingPower::Assignment)?);
    } else if explicit_type.is_none() {
        return Err(anyhow::anyhow!(
            "{}",
            "Missing type or value in variable declaration".red().bold()
        ));
    }

    parser.expect(TokenKind::Semicolon)?;

    if is_constant && assigned_value.is_none() {
        return Err(anyhow::anyhow!(
            "{}",
            "Cannot define constant without providing a value"
                .red()
                .bold()
        ));
    }

    Ok(Statement::VarDecl(VarDeclStmt {
        explicit_type,
        is_constant,
        variable_name,
        assigned_value,
    }))
}

pub fn parse_struct_decl_stmt(parser: &mut Parser) -> anyhow::Result<Statement> {
    parser.expect(TokenKind::Struct)?;
    let mut properties: Vec<StructProperty> = Vec::new();
    let methods: Vec<StructMethod> = Vec::new();
    let name = parser.expect(TokenKind::Identifier)?.value;

    parser.expect(TokenKind::OpenCurly)?;

    loop {
        if !parser.has_tokens() || parser.current_token_kind() == TokenKind::CloseCurly {
            break;
        }

        if parser.current_token_kind() == TokenKind::Identifier {
            let property_name = parser.expect(TokenKind::Identifier)?.value;
            parser.expect_error(
                TokenKind::Colon,
                Some(String::from(
                    "Expected colon after property name in struct property declaration",
                )),
            )?;
            let explicit_type = parse_type(parser, BindingPower::DefaultBp)?;

            if parser.current_token_kind() != TokenKind::CloseCurly {
                parser.expect(TokenKind::Comma)?;
            }

            if !properties
                .iter()
                .filter(|arg| arg.name == property_name)
                .collect::<Vec<_>>()
                .is_empty()
            {
                return Err(anyhow::anyhow!(
                    "{}",
                    format!("Property {property_name} has already been defined in struct")
                        .red()
                        .bold()
                ));
            }

            properties.push(StructProperty {
                name: property_name,
                explicit_type,
            });

            continue;
        }

        return Err(anyhow::anyhow!(
            "{}",
            format!(
                "Unexpected token in struct declaration: {:?}",
                parser.current_token_kind()
            )
            .red()
            .bold()
        ));
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(Statement::StructDecl(StructDeclStmt {
        name,
        properties,
        methods,
    }))
}

pub fn parse_fn_decl_stmt(parser: &mut Parser) -> anyhow::Result<Statement> {
    parser.expect(TokenKind::Fn)?;
    let mut arguments: Vec<FnArgument> = Vec::new();
    let mut body: Vec<Statement> = Vec::new();
    let name = parser.expect(TokenKind::Identifier)?.value;

    parser.expect(TokenKind::OpenParen)?;

    loop {
        if !parser.has_tokens() || parser.current_token_kind() == TokenKind::CloseParen {
            break;
        }

        if parser.current_token_kind() == TokenKind::Identifier {
            let argument_name = parser.expect(TokenKind::Identifier)?.value;
            parser.expect_error(
                TokenKind::Colon,
                Some(String::from(
                    "Expected colon after property name in function argument declaration",
                )),
            )?;
            let explicit_type = parse_type(parser, BindingPower::DefaultBp)?;

            if parser.current_token_kind() != TokenKind::CloseParen {
                parser.expect(TokenKind::Comma)?;
            }

            if !arguments
                .iter()
                .filter(|arg| arg.name == argument_name)
                .collect::<Vec<_>>()
                .is_empty()
            {
                return Err(anyhow::anyhow!(
                    "{}",
                    format!("Argument {argument_name} has already been defined in function")
                        .red()
                        .bold()
                ));
            }

            arguments.push(FnArgument {
                name: argument_name,
                explicit_type: Some(explicit_type),
            });

            continue;
        }

        return Err(anyhow::anyhow!(
            "{}",
            format!(
                "Unexpected token in function declaration: {:?}",
                parser.current_token_kind()
            )
            .red()
            .bold()
        ));
    }

    parser.expect(TokenKind::CloseParen)?;

    let explicit_type = parse_type(parser, BindingPower::DefaultBp)?;

    parser.expect(TokenKind::OpenCurly)?;

    while parser.has_tokens() && parser.current_token_kind() != TokenKind::CloseCurly {
        let stmt = parse_stmt(parser)?;
        body.push(stmt);
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(Statement::FnDecl(FnDeclStmt {
        name,
        arguments,
        body,
        explicit_type,
    }))
}

pub fn parse_return_stmt(parser: &mut Parser) -> anyhow::Result<Statement> {
    parser.expect(TokenKind::Return)?;

    let expression = if parser.current_token_kind() != TokenKind::Semicolon {
        Some(parse_expr(parser, BindingPower::DefaultBp)?)
    } else {
        None
    };

    parser.expect(TokenKind::Semicolon)?;

    Ok(Statement::Return(ReturnStmt { value: expression }))
}
