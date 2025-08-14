use std::mem;

use anyhow::{bail, Result};
use colored::Colorize;

use crate::{
    ast::{
        ast::{Expression, Statement, Type},
        statements::{
            ExpressionStmt, FnArgument, FnDeclStmt, ReturnStmt, StructDeclStmt, StructMethod,
            StructProperty, VarDeclStmt,
        },
    },
    lexer::token::Token,
    parser::{
        expr::parse_expr,
        lookups::{BindingPower, STMT_LU},
        parser::Parser,
        types::parse_type,
    },
};

pub fn parse_stmt(parser: &mut Parser) -> Result<Statement> {
    let stmt_fn = {
        let stmt_lu = STMT_LU.lock().unwrap();
        stmt_lu
            .get(&mem::discriminant(&parser.current_token()))
            .cloned()
    };

    if let Some(stmt_fn) = stmt_fn {
        stmt_fn(parser)
    } else {
        let expression = parse_expr(parser, BindingPower::DefaultBp)?;
        parser.expect(Token::Semicolon)?;

        Ok(Statement::Expression(ExpressionStmt { expression }))
    }
}

pub fn parse_var_decl_statement(parser: &mut Parser) -> Result<Statement> {
    let mut explicit_type: Option<Type> = None;
    let mut assigned_value: Option<Expression> = None;

    let is_static = match parser.current_token() {
        Token::Static => true,
        Token::Let => false,
        _ => bail!(
            "Expected 'let' or 'static' keyword, recieved {:?}",
            parser.current_token()
        ),
    };

    parser.advance();

    let is_constant = parser.current_token() != Token::Mut;

    if !is_constant {
        parser.advance();
    }

    if is_static && !is_constant {
        anyhow::bail!("Static variables must be constant");
    }

    let variable_name = parser
        .expect_error(
            Token::identifier(),
            Some(String::from(
                "Expected variable name inside variable declaration",
            )),
        )?
        .unwrap_identifier();

    if parser.current_token() == Token::Colon {
        parser.advance();
        explicit_type = Some(parse_type(parser, BindingPower::DefaultBp)?);
    }

    if parser.current_token() != Token::Semicolon {
        parser.expect(Token::Equals)?;
        assigned_value = Some(parse_expr(parser, BindingPower::Assignment)?);
    } else if explicit_type.is_none() {
        anyhow::bail!("Missing type or value in variable declaration".red().bold());
    }

    parser.expect(Token::Semicolon)?;

    if is_constant && assigned_value.is_none() {
        anyhow::bail!("Cannot define constant without providing a value"
            .red()
            .bold());
    }

    Ok(Statement::VarDecl(VarDeclStmt {
        explicit_type,
        is_constant,
        is_static,
        variable_name,
        assigned_value,
    }))
}

pub fn parse_struct_decl_stmt(parser: &mut Parser) -> Result<Statement> {
    parser.expect(Token::Struct)?;
    let mut properties: Vec<StructProperty> = Vec::new();
    let mut methods: Vec<StructMethod> = Vec::new();
    let name = parser.expect(Token::identifier())?.unwrap_identifier();

    parser.expect(Token::OpenCurly)?;

    loop {
        if !parser.has_tokens() || parser.current_token() == Token::CloseCurly {
            break;
        }

        if parser.current_token() == Token::Fn {
            let fn_decl = parse_fn_decl_stmt(parser)?;
            match fn_decl {
                Statement::FnDecl(fn_decl) => methods.push(StructMethod { fn_decl }),
                _ => unreachable!(),
            }
            continue;
        }

        if parser.current_token().eq(&Token::identifier()) {
            let is_public = if parser.current_token() == Token::Hash {
                parser.advance();
                true
            } else {
                false
            };

            let property_name = parser.expect(Token::identifier())?.unwrap_identifier();
            parser.expect_error(
                Token::Colon,
                Some(String::from(
                    "Expected colon after property name in struct property declaration",
                )),
            )?;
            let explicit_type = parse_type(parser, BindingPower::DefaultBp)?;

            if parser.current_token() != Token::CloseCurly {
                parser.expect(Token::Comma)?;
            }

            if !properties
                .iter()
                .filter(|arg| arg.name == property_name)
                .collect::<Vec<_>>()
                .is_empty()
            {
                bail!(
                    format!("Property {property_name} has already been defined in struct")
                        .red()
                        .bold()
                );
            }

            properties.push(StructProperty {
                name: property_name,
                explicit_type,
                is_public,
            });

            continue;
        }

        bail!(format!(
            "Unexpected token in struct declaration: {:?}",
            parser.current_token()
        )
        .red()
        .bold());
    }

    parser.expect(Token::CloseCurly)?;

    Ok(Statement::StructDecl(StructDeclStmt {
        name,
        properties,
        methods,
        is_public: false,
    }))
}

pub fn parse_pub_stmt(parser: &mut Parser) -> Result<Statement> {
    parser.expect(Token::Pub)?;
    let mut stmt = parse_stmt(parser)?;
    match &mut stmt {
        Statement::StructDecl(struct_decl_stmt) => {
            struct_decl_stmt.is_public = true;
        }
        Statement::FnDecl(fn_decl_stmt) => {
            fn_decl_stmt.is_public = true;
        }
        _ => return Err(anyhow::anyhow!("Expected function or struct declaration")),
    }
    Ok(stmt)
}

pub fn parse_fn_decl_stmt(parser: &mut Parser) -> Result<Statement> {
    parser.expect(Token::Fn)?;
    let mut arguments: Vec<FnArgument> = Vec::new();
    let mut body: Vec<Statement> = Vec::new();
    let name = parser.expect(Token::identifier())?.unwrap_identifier();

    parser.expect(Token::OpenParen)?;

    loop {
        if !parser.has_tokens() || parser.current_token() == Token::CloseParen {
            break;
        }

        if parser.current_token().eq(&Token::identifier()) {
            let argument_name = parser.expect(Token::identifier())?.unwrap_identifier();
            parser.expect_error(
                Token::Colon,
                Some(String::from(
                    "Expected colon after property name in function argument declaration",
                )),
            )?;
            let explicit_type = parse_type(parser, BindingPower::DefaultBp)?;

            if parser.current_token() != Token::CloseParen {
                parser.expect(Token::Comma)?;
            }

            if !arguments
                .iter()
                .filter(|arg| arg.name == argument_name)
                .collect::<Vec<_>>()
                .is_empty()
            {
                bail!(
                    format!("Argument {argument_name} has already been defined in function")
                        .red()
                        .bold()
                );
            }

            arguments.push(FnArgument {
                name: argument_name,
                explicit_type: Some(explicit_type),
            });

            continue;
        }

        bail!(format!(
            "Unexpected token in function declaration: {:?}",
            parser.current_token()
        )
        .red()
        .bold());
    }

    parser.expect(Token::CloseParen)?;

    let explicit_type = parse_type(parser, BindingPower::DefaultBp)?;

    parser.expect(Token::OpenCurly)?;

    while parser.has_tokens() && parser.current_token() != Token::CloseCurly {
        let stmt = parse_stmt(parser)?;
        body.push(stmt);
    }

    parser.expect(Token::CloseCurly)?;

    Ok(Statement::FnDecl(FnDeclStmt {
        name,
        arguments,
        body,
        explicit_type,
        is_public: false,
    }))
}

pub fn parse_return_stmt(parser: &mut Parser) -> Result<Statement> {
    parser.expect(Token::Return)?;

    let expression = if parser.current_token() != Token::Semicolon {
        Some(parse_expr(parser, BindingPower::DefaultBp)?)
    } else {
        None
    };

    parser.expect(Token::Semicolon)?;

    Ok(Statement::Return(ReturnStmt { value: expression }))
}
