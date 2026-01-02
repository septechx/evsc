use anyhow::Result;

use crate::{
    ERRORS,
    ast::{
        Attribute, Expr, ImportTree, ImportTreeKind, NodeId, Stmt, StmtKind, Type, TypeKind,
        statements::{
            ExpressionStmt, FnArgument, FnDeclStmt, ImportStmt, InterfaceDeclStmt, InterfaceMethod,
            ReturnStmt, StructDeclStmt, StructMethod, StructProperty, VarDeclStmt,
        },
    },
    errors::{
        builders,
        widgets::{CodeWidget, LocationWidget},
    },
    get_modifiers,
    lexer::token::TokenKind,
    parser::{
        Parser,
        attributes::parse_attributes,
        expr::parse_expr,
        lookups::{BindingPower, STMT_LU},
        modifiers::{Modifier, ModifierKind, parse_modifiers},
        types::parse_type,
        utils::{parse_path, parse_rename, unexpected_token},
    },
    span::Span,
};

pub fn parse_stmt(parser: &mut Parser) -> Result<Stmt> {
    let attributes = parse_attributes(parser)?;
    let modifiers = parse_modifiers(parser);
    let stmt_lu = STMT_LU.get().expect("Lookups not initialized");

    let stmt_fn = stmt_lu.get(&parser.current_token().kind).cloned();

    if let Some(stmt_fn) = stmt_fn {
        stmt_fn(parser, attributes, modifiers)
    } else {
        if !attributes.is_empty() {
            crate::ERRORS.with(|e| -> Result<()> {
                e.borrow_mut().add(
                    builders::error("Attribute not allowed here")
                        .add_widget(LocationWidget::new(
                            attributes[0].span,
                            parser.current_token().module_id,
                        )?)
                        .add_widget(CodeWidget::new(
                            attributes[0].span,
                            parser.current_token().module_id,
                        )?),
                );
                Ok(())
            })?;
        }

        if !modifiers.is_empty() {
            crate::ERRORS.with(|e| -> Result<()> {
                e.borrow_mut().add(
                    builders::error("Modifier not allowed here")
                        .add_widget(LocationWidget::new(
                            modifiers[0].span,
                            parser.current_token().module_id,
                        )?)
                        .add_widget(CodeWidget::new(
                            modifiers[0].span,
                            parser.current_token().module_id,
                        )?),
                );
                Ok(())
            })?;
        }

        let expression = parse_expr(parser, BindingPower::DefaultBp)?;
        parser.expect(TokenKind::Semicolon)?;

        let span = expression.span;
        Ok(parser.stmt(
            StmtKind::Expression(ExpressionStmt { expression }),
            span,
            vec![],
        ))
    }
}

pub fn parse_var_decl_statement(
    parser: &mut Parser,
    attributes: Vec<Attribute>,
    modifiers: Vec<Modifier>,
) -> Result<Stmt> {
    if !attributes.is_empty() {
        crate::ERRORS.with(|e| -> Result<()> {
            e.borrow_mut().add(
                builders::error("Attribute not allowed here")
                    .add_widget(LocationWidget::new(
                        attributes[0].span,
                        parser.current_token().module_id,
                    )?)
                    .add_widget(CodeWidget::new(
                        attributes[0].span,
                        parser.current_token().module_id,
                    )?),
            );
            Ok(())
        })?;
    }

    let var_token = parser.advance();
    let mut type_: Type = parser.type_(
        TypeKind::Infer,
        Span::new(var_token.span.end(), var_token.span.end()),
    );
    let mut assigned_value: Option<Expr> = None;

    let is_static = var_token.kind == TokenKind::Static;

    let is_constant = parser.current_token().kind != TokenKind::Mut;

    if is_static && !is_constant {
        let span = parser.current_token().span;

        crate::ERRORS.with(|e| -> Result<()> {
            e.borrow_mut().add(
                builders::error("Static variables must be constant")
                    .add_widget(LocationWidget::new(span, parser.current_token().module_id)?)
                    .add_widget(CodeWidget::new(span, parser.current_token().module_id)?),
            );
            Ok(())
        })?;
    }

    if !is_constant {
        parser.advance();
    }

    let variable_name = parser.expect_identifier()?;

    if parser.current_token().kind == TokenKind::Colon {
        parser.advance();
        type_ = parse_type(parser, BindingPower::DefaultBp)?;
    }

    if parser.current_token().kind != TokenKind::Semicolon {
        parser.expect(TokenKind::Equals)?;
        assigned_value = Some(parse_expr(parser, BindingPower::Assignment)?);
    }

    let (pub_mod,) = get_modifiers!(&parser, modifiers, [Pub]);

    let mut is_public = false;

    let end_span = parser.expect(TokenKind::Semicolon)?.span;
    let mut start_span = var_token.span;

    if let Some(pub_mod) = pub_mod {
        if !is_static {
            crate::ERRORS.with(|e| -> Result<()> {
                e.borrow_mut().add(
                    builders::error("Modifier 'pub' is only allowed on static variables")
                        .add_widget(LocationWidget::new(
                            pub_mod.span,
                            parser.current_token().module_id,
                        )?)
                        .add_widget(CodeWidget::new(
                            pub_mod.span,
                            parser.current_token().module_id,
                        )?),
                );
                Ok(())
            })?;
        }

        start_span = pub_mod.span;
        is_public = true;
    };

    let span = Span::new(start_span.start(), end_span.end());

    if assigned_value.is_none() && is_constant {
        crate::ERRORS.with(|e| -> Result<()> {
            e.borrow_mut().add(
                builders::warning("Declared constant without providing a value")
                    .add_widget(LocationWidget::new(span, parser.current_token().module_id)?)
                    .add_widget(CodeWidget::new(span, parser.current_token().module_id)?),
            );
            Ok(())
        })?;
    }

    Ok(parser.stmt(
        StmtKind::VarDecl(VarDeclStmt {
            type_,
            is_constant,
            is_static,
            is_public,
            variable_name,
            assigned_value,
        }),
        span,
        vec![],
    ))
}

pub fn parse_struct_decl_stmt(
    parser: &mut Parser,
    attributes: Vec<Attribute>,
    modifiers: Vec<Modifier>,
) -> Result<Stmt> {
    let struct_token = parser.expect(TokenKind::Struct)?;
    let mut properties: Vec<StructProperty> = Vec::new();
    let mut methods: Vec<StructMethod> = Vec::new();
    let name = parser.expect_identifier()?;

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
            if let StmtKind::FnDecl(fn_decl) = parse_fn_decl_stmt(parser, vec![], vec![])?.kind {
                methods.push(StructMethod {
                    fn_decl: FnDeclStmt {
                        is_extern: false,
                        is_public,
                        ..fn_decl
                    },
                    is_static,
                    is_public,
                })
            };
            continue;
        }

        if is_static {
            let span = parser.current_token().span;
            let module_id = parser.current_token().module_id;

            crate::ERRORS.with(|e| -> Result<()> {
                e.borrow_mut().add(
                    builders::error("Only struct methods are allowed to be static")
                        .add_widget(LocationWidget::new(span, module_id)?)
                        .add_widget(CodeWidget::new(span, module_id)?),
                );
                Ok(())
            })?;
        }

        if parser.current_token().kind == TokenKind::Identifier {
            let property_name = parser.expect_identifier()?;
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
                .filter(|arg| arg.name.value == property_name.value)
                .collect::<Vec<_>>()
                .is_empty()
            {
                crate::ERRORS.with(|e| -> Result<()> {
                    e.borrow_mut().add(
                        builders::error(format!(
                            "Property {} has already been defined in struct",
                            property_name.value
                        ))
                        .add_widget(LocationWidget::new(
                            property_name.span,
                            parser.current_token().module_id,
                        )?)
                        .add_widget(CodeWidget::new(
                            property_name.span,
                            parser.current_token().module_id,
                        )?),
                    );
                    Ok(())
                })?;
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

    let end_span = parser.expect(TokenKind::CloseCurly)?.span;

    let (pub_mod,) = get_modifiers!(&parser, modifiers, [Pub]);

    let mut is_public = false;

    let mut start_span = struct_token.span;

    if let Some(pub_mod) = pub_mod {
        start_span = pub_mod.span;
        is_public = true;
    };

    let span = Span::new(start_span.start(), end_span.end());

    Ok(parser.stmt(
        StmtKind::StructDecl(StructDeclStmt {
            name,
            properties,
            methods,
            is_public,
        }),
        span,
        attributes,
    ))
}

pub fn parse_interface_decl_stmt(
    parser: &mut Parser,
    attributes: Vec<Attribute>,
    modifiers: Vec<Modifier>,
) -> Result<Stmt> {
    let interface_token = parser.expect(TokenKind::Interface)?;
    let mut methods: Vec<InterfaceMethod> = Vec::new();
    let name = parser.expect_identifier()?;

    parser.expect(TokenKind::OpenCurly)?;

    loop {
        if !parser.has_tokens() || parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        if parser.current_token().kind == TokenKind::Fn {
            if let StmtKind::FnDecl(fn_decl) = parse_fn_decl_stmt(parser, vec![], vec![])?.kind {
                methods.push(InterfaceMethod { fn_decl });
            }
            continue;
        }

        unexpected_token(parser.current_token());
    }

    let end_span = parser.expect(TokenKind::CloseCurly)?.span;

    let (pub_mod,) = get_modifiers!(&parser, modifiers, [Pub]);

    let mut is_public = false;

    let mut start_span = interface_token.span;

    if let Some(pub_mod) = pub_mod {
        start_span = pub_mod.span;
        is_public = true;
    };

    let span = Span::new(start_span.start(), end_span.end());

    Ok(parser.stmt(
        StmtKind::InterfaceDecl(InterfaceDeclStmt {
            name,
            methods,
            is_public,
        }),
        span,
        attributes,
    ))
}

pub fn parse_fn_decl_stmt(
    parser: &mut Parser,
    attributes: Vec<Attribute>,
    modifiers: Vec<Modifier>,
) -> Result<Stmt> {
    let (pub_mod, extern_mod) = get_modifiers!(&parser, modifiers, [Pub, Extern]);

    let fn_token = parser.expect(TokenKind::Fn)?;
    let name = parser.expect_identifier()?;

    parser.expect(TokenKind::OpenParen)?;
    let mut arguments: Vec<FnArgument> = vec![];

    loop {
        if parser.current_token().kind == TokenKind::CloseParen {
            break;
        }

        let arg_name = parser.expect_identifier()?;

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

    let mut end_span = parser.expect(TokenKind::CloseParen)?.span;

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

        end_span = parser.expect(TokenKind::CloseCurly)?.span;
    }

    let mut start_span = fn_token.span;

    if let Some(pub_mod) = pub_mod {
        start_span = pub_mod.span;
    } else if let Some(extern_mod) = extern_mod {
        start_span = extern_mod.span;
    }

    let span = Span::new(start_span.start(), end_span.end());

    Ok(parser.stmt(
        StmtKind::FnDecl(FnDeclStmt {
            name,
            arguments,
            body,
            return_type,
            is_public: pub_mod.is_some(),
            is_extern: extern_mod.is_some(),
        }),
        span,
        attributes,
    ))
}

pub fn parse_return_stmt(
    parser: &mut Parser,
    _attributes: Vec<Attribute>,
    _modifiers: Vec<Modifier>,
) -> Result<Stmt> {
    let return_token = parser.advance();

    let value = if parser.current_token().kind != TokenKind::Semicolon {
        Some(parse_expr(parser, BindingPower::DefaultBp)?)
    } else {
        None
    };

    let end_span = parser.expect(TokenKind::Semicolon)?.span;

    let span = Span::new(return_token.span.start(), end_span.end());

    Ok(parser.stmt(StmtKind::Return(ReturnStmt { value }), span, vec![]))
}

pub fn parse_import_stmt(
    parser: &mut Parser,
    attributes: Vec<Attribute>,
    modifiers: Vec<Modifier>,
) -> Result<Stmt> {
    if !modifiers.is_empty() {
        crate::ERRORS.with(|e| -> Result<()> {
            e.borrow_mut().add(
                builders::error("Modifier not allowed here")
                    .add_widget(LocationWidget::new(
                        attributes[0].span,
                        parser.current_token().module_id,
                    )?)
                    .add_widget(CodeWidget::new(
                        attributes[0].span,
                        parser.current_token().module_id,
                    )?),
            );
            Ok(())
        })?;
    }

    let start_span = parser.expect(TokenKind::Import)?.span;
    let tree = parse_import_tree(parser)?;
    let end_span = parser.expect(TokenKind::Semicolon)?.span;

    let span = Span::new(start_span.start(), end_span.end());
    Ok(parser.stmt(StmtKind::Import(ImportStmt { tree }), span, attributes))
}

fn parse_import_tree(parser: &mut Parser) -> Result<ImportTree> {
    let prefix = parse_path(parser)?;
    let kind = if parser.current_token().kind == TokenKind::ColonColon {
        parser.advance();
        if parser.current_token().kind == TokenKind::Star {
            parser.advance();
            ImportTreeKind::Glob
        } else {
            ImportTreeKind::Nested {
                items: parse_import_tree_list(parser)?,
                span: Span::new(prefix.span.start(), parser.current_token().span.end()),
            }
        }
    } else {
        ImportTreeKind::Simple(parse_rename(parser)?)
    };

    let span = Span::new(prefix.span.start(), parser.current_token().span.end() - 1);

    Ok(ImportTree { prefix, kind, span })
}

fn parse_import_tree_list(parser: &mut Parser) -> Result<Vec<(ImportTree, NodeId)>> {
    let mut items = vec![];

    parser.expect(TokenKind::OpenCurly)?;

    loop {
        let tree = parse_import_tree(parser)?;
        let id = parser.next_id();

        items.push((tree, id));

        if parser.peek().kind != TokenKind::CloseCurly
            && parser.current_token().kind == TokenKind::Comma
        {
            parser.expect(TokenKind::Comma)?;
        }

        if parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(items)
}
