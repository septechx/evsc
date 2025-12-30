use anyhow::{Result, bail};

use crate::{
    ast::{
        Attribute, Expr, Stmt, StmtKind, Type,
        statements::{
            ExpressionStmt, FnArgument, FnDeclStmt, InterfaceDeclStmt, InterfaceMethod, ReturnStmt,
            StructDeclStmt, StructMethod, StructProperty, VarDeclStmt,
        },
    },
    errors::{CodeType, builders},
    lexer::token::TokenKind,
    parser::{
        Parser,
        attributes::parse_attributes,
        expr::parse_expr,
        lookups::{BindingPower, STMT_LU},
        types::parse_type,
        utils::unexpected_token,
    },
    span::{Span, sourcemaps::get_code_line},
};

pub fn parse_stmt(parser: &mut Parser) -> Result<Stmt> {
    let attributes = parse_attributes(parser)?;
    parse_stmt_with_attrs(parser, attributes)
}

fn parse_stmt_with_attrs(parser: &mut Parser, attributes: Vec<Attribute>) -> Result<Stmt> {
    let stmt_lu = STMT_LU.get().expect("Lookups not initialized");

    let stmt_fn = stmt_lu.get(&parser.current_token().kind).cloned();

    if let Some(stmt_fn) = stmt_fn {
        stmt_fn(parser, attributes)
    } else {
        let expression = parse_expr(parser, BindingPower::DefaultBp)?;
        parser.expect(TokenKind::Semicolon)?;

        let span = expression.span;
        Ok(parser.stmt(StmtKind::Expression(ExpressionStmt { expression }), span))
    }
}

pub fn parse_var_decl_statement(parser: &mut Parser, _attributes: Vec<Attribute>) -> Result<Stmt> {
    let var_token = parser.advance();
    let mut type_: Type = Type::Infer;
    let mut assigned_value: Option<Expr> = None;

    let is_static = match var_token.kind {
        TokenKind::Static => true,
        TokenKind::Let => false,
        _ => bail!(
            "Expected 'let' or 'static' keyword, recieved {:?}",
            parser.current_token()
        ),
    };

    let is_constant = parser.current_token().kind != TokenKind::Mut;

    if !is_constant {
        parser.advance();
    }

    if is_static && !is_constant {
        anyhow::bail!("Static variables must be constant");
    }

    let variable_name = parser
        .expect_error(
            TokenKind::Identifier,
            Some(String::from(
                "Expected variable name inside variable declaration",
            )),
        )?
        .value;

    if parser.current_token().kind == TokenKind::Colon {
        parser.advance();
        type_ = parse_type(parser, BindingPower::DefaultBp)?;
    }

    if parser.current_token().kind != TokenKind::Semicolon {
        parser.expect(TokenKind::Equals)?;
        assigned_value = Some(parse_expr(parser, BindingPower::Assignment)?);
    }

    let end_token = parser.expect(TokenKind::Semicolon)?;

    let span = Span::new(var_token.span.start(), end_token.span.end());

    if assigned_value.is_none()
        && let Type::Infer = type_
    {
        let code_line = get_code_line(parser.current_token().module_id, span, CodeType::None);

        crate::ERRORS.with(|e| {
            e.borrow_mut().add(
                builders::error("Missing type or value in variable declaration")
                    .with_span(span, parser.current_token().module_id)
                    .with_code(code_line),
            );
        });
    }

    if assigned_value.is_none() && is_constant {
        let code_line = get_code_line(parser.current_token().module_id, span, CodeType::None);

        crate::ERRORS.with(|e| {
            e.borrow_mut().add(
                builders::error("Cannot define constant without providing a value")
                    .with_span(span, parser.current_token().module_id)
                    .with_code(code_line),
            );
        });
    }

    Ok(parser.stmt(
        StmtKind::VarDecl(VarDeclStmt {
            type_,
            is_constant,
            is_static,
            variable_name,
            assigned_value,
        }),
        span,
    ))
}

pub fn parse_struct_decl_stmt(parser: &mut Parser, attributes: Vec<Attribute>) -> Result<Stmt> {
    let struct_token = parser.expect(TokenKind::Struct)?;
    let mut properties: Vec<StructProperty> = Vec::new();
    let mut methods: Vec<StructMethod> = Vec::new();
    let name = parser.expect(TokenKind::Identifier)?.value;

    parser.expect(TokenKind::OpenCurly)?;

    loop {
        if !parser.has_tokens() || parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        let is_public = parser.current_token().kind == TokenKind::Pub;
        if is_public {
            parser.advance();
        }

        let is_static = parser.current_token().kind == TokenKind::Static;
        if is_static {
            parser.advance();
        }

        if parser.current_token().kind == TokenKind::Fn {
            if let StmtKind::FnDecl(fn_decl) = parse_fn_decl_stmt(parser, vec![])?.kind {
                methods.push(StructMethod {
                    fn_decl: FnDeclStmt {
                        is_extern: false,
                        is_public,
                        ..fn_decl
                    },
                    is_static,
                })
            };
            continue;
        }

        if is_static {
            let span = parser.current_token().span;
            let module_id = parser.current_token().module_id;

            let code_line = get_code_line(module_id, span, CodeType::None);

            crate::ERRORS.with(|e| {
                e.borrow_mut().add(
                    builders::error("Only struct methods are allowed to be static")
                        .with_span(span, module_id)
                        .with_code(code_line),
                );
            })
        }

        if parser.current_token().kind == TokenKind::Identifier {
            let property = parser.expect(TokenKind::Identifier)?;
            let property_name = property.value;
            parser.expect_error(
                TokenKind::Colon,
                Some(String::from(
                    "Expected colon after property name in struct property declaration",
                )),
            )?;
            let type_ = parse_type(parser, BindingPower::DefaultBp)?;

            if parser.current_token().kind != TokenKind::CloseCurly {
                parser.expect(TokenKind::Comma)?;
            }

            if !properties
                .iter()
                .filter(|arg| arg.name == property_name)
                .collect::<Vec<_>>()
                .is_empty()
            {
                let code_line = get_code_line(
                    parser.current_token().module_id,
                    property.span,
                    CodeType::None,
                );

                crate::ERRORS.with(|e| {
                    e.borrow_mut().add(
                        builders::error(format!(
                            "Property {property_name} has already been defined in struct"
                        ))
                        .with_span(property.span, parser.current_token().module_id)
                        .with_code(code_line),
                    );
                });
            }

            properties.push(StructProperty {
                name: property_name,
                type_,
                is_public,
            });

            continue;
        }

        unexpected_token(parser.current_token());
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(parser.stmt(
        StmtKind::StructDecl(StructDeclStmt {
            name,
            properties,
            methods,
            is_public: attributes.iter().any(|a| a.name == "pub"),
            attributes,
        }),
        struct_token.span,
    ))
}

pub fn parse_interface_decl_stmt(parser: &mut Parser, attributes: Vec<Attribute>) -> Result<Stmt> {
    let interface_token = parser.expect(TokenKind::Interface)?;
    let mut methods: Vec<InterfaceMethod> = Vec::new();
    let name = parser.expect(TokenKind::Identifier)?.value;

    parser.expect(TokenKind::OpenCurly)?;

    loop {
        if !parser.has_tokens() || parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        if parser.current_token().kind == TokenKind::Fn {
            if let StmtKind::FnDecl(fn_decl) = parse_fn_decl_stmt(parser, vec![])?.kind {
                methods.push(InterfaceMethod { fn_decl });
            }
            continue;
        }

        unexpected_token(parser.current_token());
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(parser.stmt(
        StmtKind::InterfaceDecl(InterfaceDeclStmt {
            name,
            methods,
            is_public: attributes.iter().any(|a| a.name == "pub"),
            attributes,
        }),
        interface_token.span,
    ))
}

pub fn parse_fn_decl_stmt(parser: &mut Parser, attributes: Vec<Attribute>) -> Result<Stmt> {
    let fn_token = parser.expect(TokenKind::Fn)?;
    let name = parser.expect(TokenKind::Identifier)?.value;

    parser.expect(TokenKind::OpenParen)?;
    let mut arguments: Vec<FnArgument> = vec![];

    loop {
        if parser.current_token().kind == TokenKind::CloseParen {
            break;
        }

        let arg_name = parser.expect(TokenKind::Identifier)?.value;

        parser.expect(TokenKind::Colon)?;
        let type_ = parse_type(parser, BindingPower::DefaultBp)?;

        arguments.push(FnArgument {
            name: arg_name,
            type_,
        });

        if parser.current_token().kind == TokenKind::Comma {
            parser.advance();
        }
    }

    parser.expect(TokenKind::CloseParen)?;

    let return_type = parse_type(parser, BindingPower::DefaultBp)?;

    let mut body: Vec<Stmt> = vec![];

    if parser.current_token().kind == TokenKind::OpenCurly {
        parser.expect(TokenKind::OpenCurly)?;

        loop {
            if parser.current_token().kind == TokenKind::CloseCurly {
                break;
            }

            body.push(parse_stmt(parser)?);
        }

        parser.expect(TokenKind::CloseCurly)?;
    }

    Ok(parser.stmt(
        StmtKind::FnDecl(FnDeclStmt {
            name,
            arguments,
            body,
            return_type,
            is_public: attributes.iter().any(|a| a.name == "pub"),
            is_extern: attributes.iter().any(|a| a.name == "extern"),
            attributes,
        }),
        fn_token.span,
    ))
}

pub fn parse_return_stmt(parser: &mut Parser, _attributes: Vec<Attribute>) -> Result<Stmt> {
    let return_token = parser.advance();

    let value = if parser.current_token().kind != TokenKind::Semicolon {
        Some(parse_expr(parser, BindingPower::DefaultBp)?)
    } else {
        None
    };

    parser.expect(TokenKind::Semicolon)?;

    Ok(parser.stmt(StmtKind::Return(ReturnStmt { value }), return_token.span))
}

pub fn parse_pub_stmt(parser: &mut Parser, attributes: Vec<Attribute>) -> Result<Stmt> {
    parser.expect(TokenKind::Pub)?;

    let mut stmt = parse_stmt_with_attrs(parser, attributes)?;
    match &mut stmt.kind {
        StmtKind::StructDecl(struct_decl_stmt) => {
            struct_decl_stmt.is_public = true;
        }
        StmtKind::FnDecl(fn_decl_stmt) => {
            fn_decl_stmt.is_public = true;
        }
        _ => return Err(anyhow::anyhow!("Expected function or struct declaration")),
    }
    Ok(stmt)
}
