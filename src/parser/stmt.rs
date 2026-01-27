use anyhow::Result;
use thin_vec::ThinVec;

use crate::{
    ast::{
        Attribute, Block, Expr, ExprKind, ImportTree, ImportTreeKind, Mutability, Stmt, StmtKind,
        Type, TypeKind, Visibility,
        statements::{
            ExprStmt, FnDeclStmt, FnParameter, ImplStmt, ImportStmt, InterfaceDeclStmt,
            InterfaceMethod, ReturnStmt, SemiStmt, StructDeclStmt, StructField, StructMethod,
            VarDeclStmt,
        },
    },
    error_at, get_modifiers,
    lexer::token::TokenKind,
    no_attributes, no_modifiers,
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
    warning_at,
};

pub fn parse_stmt(parser: &mut Parser) -> Result<Stmt> {
    let attributes = parse_attributes(parser)?;
    let modifiers = parse_modifiers(parser);
    let stmt_lu = STMT_LU.get().expect("Lookups not initialized");

    let stmt_fn = stmt_lu.get(&parser.current_token().kind).cloned();

    if let Some(stmt_fn) = stmt_fn {
        stmt_fn(parser, attributes, modifiers)
    } else {
        no_attributes!(&parser, &attributes);
        no_modifiers!(&parser, &modifiers);

        let expr = parse_expr(parser, BindingPower::DefaultBp)?;

        let mut has_semicolon = false;
        if parser.current_token().kind == TokenKind::Semicolon {
            has_semicolon = true;
            parser.advance();
        }

        let span = expr.span;
        let kind = if has_semicolon {
            StmtKind::Semi(SemiStmt { expr })
        } else {
            StmtKind::Expr(ExprStmt { expr })
        };

        Ok(Stmt {
            kind,
            attributes,
            span,
        })
    }
}

pub fn parse_var_decl_statement(
    parser: &mut Parser,
    attributes: ThinVec<Attribute>,
    modifiers: ThinVec<Modifier>,
) -> Result<Stmt> {
    no_attributes!(&parser, &attributes);

    let var_token = parser.advance();
    let mut type_ = Type {
        kind: TypeKind::Infer,
        span: Span::new(var_token.span.end(), var_token.span.end()),
    };
    let mut assigned_value: Option<Expr> = None;

    let is_static = var_token.kind == TokenKind::Static;

    let is_constant = parser.current_token().kind != TokenKind::Mut;

    if is_static && !is_constant {
        error_at!(
            parser.current_token().span,
            parser.current_token().module_id,
            "Static variables must be constant"
        )?;
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
            error_at!(
                pub_mod.span,
                parser.current_token().module_id,
                "Modifier 'pub' is only allowed on static variables"
            )?;
        }

        start_span = pub_mod.span;
        is_public = true;
    };

    let span = Span::new(start_span.start(), end_span.end());

    if assigned_value.is_none() && is_constant {
        warning_at!(
            span,
            parser.current_token().module_id,
            "Declared constant without providing a value"
        )?;
    }

    let mutability = if is_constant {
        Mutability::Constant
    } else {
        Mutability::Mutable
    };
    let visibility = if is_public {
        Visibility::Public
    } else {
        Visibility::Private
    };

    Ok(Stmt {
        kind: StmtKind::VarDecl(VarDeclStmt {
            ty: type_,
            is_static,
            variable_name,
            assigned_value,
            mutability,
            visibility,
        }),
        attributes,
        span,
    })
}

pub fn parse_struct_decl_stmt(
    parser: &mut Parser,
    attributes: ThinVec<Attribute>,
    modifiers: ThinVec<Modifier>,
) -> Result<Stmt> {
    let struct_token = parser.expect(TokenKind::Struct)?;
    let mut fields: ThinVec<StructField> = ThinVec::new();
    let mut methods: ThinVec<StructMethod> = ThinVec::new();
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

        let visibility = if is_public {
            Visibility::Public
        } else {
            Visibility::Private
        };

        if parser.current_token().kind == TokenKind::Fn {
            if let StmtKind::FnDecl(fn_decl) =
                parse_fn_decl_stmt(parser, ThinVec::new(), ThinVec::new())?.kind
            {
                methods.push(StructMethod {
                    fn_decl: FnDeclStmt {
                        is_extern: false,
                        visibility,
                        ..fn_decl
                    },
                    is_static,
                    visibility,
                })
            };
            continue;
        }

        if is_static {
            error_at!(
                parser.current_token().span,
                parser.current_token().module_id,
                "Only struct methods are allowed to be static"
            )?;
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

            if fields
                .iter()
                .any(|arg| arg.name.value == property_name.value)
            {
                error_at!(
                    property_name.span,
                    parser.current_token().module_id,
                    format!(
                        "Property {} has already been defined in struct",
                        property_name.value
                    )
                )?;
                continue;
            }

            let visibility = if is_public {
                Visibility::Public
            } else {
                Visibility::Private
            };

            fields.push(StructField {
                name: property_name,
                ty: type_,
                visibility,
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

    let visibility = if is_public {
        Visibility::Public
    } else {
        Visibility::Private
    };

    Ok(Stmt {
        kind: StmtKind::StructDecl(StructDeclStmt {
            fields,
            methods,
            name,
            visibility,
        }),
        attributes,
        span,
    })
}

pub fn parse_interface_decl_stmt(
    parser: &mut Parser,
    attributes: ThinVec<Attribute>,
    modifiers: ThinVec<Modifier>,
) -> Result<Stmt> {
    let interface_token = parser.expect(TokenKind::Interface)?;
    let name = parser.expect_identifier()?;

    let mut methods: ThinVec<InterfaceMethod> = ThinVec::new();
    parser.expect(TokenKind::OpenCurly)?;
    loop {
        if !parser.has_tokens() || parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        if parser.current_token().kind == TokenKind::Fn
            && let StmtKind::FnDecl(fn_decl) =
                parse_fn_decl_stmt(parser, ThinVec::new(), ThinVec::new())?.kind
        {
            methods.push(InterfaceMethod { fn_decl });
        } else if parser.current_token().kind == TokenKind::Comma {
            parser.advance();
        } else {
            unexpected_token(parser.current_token());
        }
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

    let visibility = if is_public {
        Visibility::Public
    } else {
        Visibility::Private
    };

    Ok(Stmt {
        kind: StmtKind::InterfaceDecl(InterfaceDeclStmt {
            methods,
            name,
            visibility,
        }),
        attributes,
        span,
    })
}

pub fn parse_fn_decl_stmt(
    parser: &mut Parser,
    attributes: ThinVec<Attribute>,
    modifiers: ThinVec<Modifier>,
) -> Result<Stmt> {
    let (pub_mod, extern_mod) = get_modifiers!(&parser, modifiers, [Pub, Extern]);

    let fn_token = parser.expect(TokenKind::Fn)?;
    let name = parser.expect_identifier()?;

    parser.expect(TokenKind::OpenParen)?;
    let mut parameters: ThinVec<FnParameter> = ThinVec::new();

    loop {
        if parser.current_token().kind == TokenKind::CloseParen {
            break;
        }

        let arg_name = parser.expect_identifier()?;

        parser.expect(TokenKind::Colon)?;
        let type_ = parse_type(parser, BindingPower::DefaultBp)?;

        parameters.push(FnParameter {
            name: arg_name,
            ty: type_,
        });

        if parser.current_token().kind == TokenKind::Comma {
            parser.advance();
        }
    }

    parser.expect(TokenKind::CloseParen)?;

    let return_type = parse_type(parser, BindingPower::DefaultBp)?;
    let mut end_span = return_type.span;

    // TODO: Don't try to parse function body as block expr
    let mut body: Option<Block> = None;
    if parser.current_token().kind == TokenKind::OpenCurly {
        let expr = parse_expr(parser, BindingPower::DefaultBp)?;
        end_span = expr.span;
        if let ExprKind::Block(b) = expr.kind {
            body = Some(b.block);
        } else {
            error_at!(
                expr.span,
                parser.current_token().module_id,
                "Expected block as function body"
            )?;
        }
    } else {
        match parser.current_token().kind {
            TokenKind::Semicolon => {
                end_span = parser.expect(TokenKind::Semicolon)?.span;
            }
            TokenKind::Comma => {
                end_span = parser.peek().span;
                // Token is consumed by the caller (interface)
            }
            _ => {
                error_at!(
                    parser.current_token().span,
                    parser.current_token().module_id,
                    "Expected function body or terminator after signature"
                )?;
            }
        }
    }

    let mut start_span = fn_token.span;

    if let Some(pub_mod) = pub_mod {
        start_span = pub_mod.span;
    } else if let Some(extern_mod) = extern_mod {
        start_span = extern_mod.span;
    }

    let span = Span::new(start_span.start(), end_span.end());

    let visibility = if pub_mod.is_some() {
        Visibility::Public
    } else {
        Visibility::Private
    };

    Ok(Stmt {
        kind: StmtKind::FnDecl(FnDeclStmt {
            parameters,
            body,
            name,
            return_type,
            visibility,
            is_extern: extern_mod.is_some(),
        }),
        attributes,
        span,
    })
}

pub fn parse_return_stmt(
    parser: &mut Parser,
    attributes: ThinVec<Attribute>,
    modifiers: ThinVec<Modifier>,
) -> Result<Stmt> {
    no_attributes!(&parser, &attributes);
    no_modifiers!(&parser, &modifiers);

    let return_token = parser.advance();

    let value = if parser.current_token().kind != TokenKind::Semicolon {
        Some(parse_expr(parser, BindingPower::DefaultBp)?)
    } else {
        None
    };

    let end_span = parser.expect(TokenKind::Semicolon)?.span;

    let span = Span::new(return_token.span.start(), end_span.end());

    Ok(Stmt {
        kind: StmtKind::Return(ReturnStmt { value }),
        attributes,
        span,
    })
}

pub fn parse_impl_stmt(
    parser: &mut Parser,
    attributes: ThinVec<Attribute>,
    modifiers: ThinVec<Modifier>,
) -> Result<Stmt> {
    no_attributes!(&parser, &attributes);
    no_modifiers!(&parser, &modifiers);

    let start_span = parser.expect(TokenKind::Impl)?.span;
    let interface = parser.expect_identifier()?;
    parser.expect(TokenKind::Colon)?;
    let self_ty = parse_type(parser, BindingPower::DefaultBp)?;

    let mut methods: ThinVec<InterfaceMethod> = ThinVec::new();
    parser.expect(TokenKind::OpenCurly)?;
    loop {
        if !parser.has_tokens() || parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        if parser.current_token().kind == TokenKind::Fn
            && let StmtKind::FnDecl(fn_decl) =
                parse_fn_decl_stmt(parser, ThinVec::new(), ThinVec::new())?.kind
        {
            methods.push(InterfaceMethod { fn_decl });
        } else {
            unexpected_token(parser.current_token());
        }
    }
    let end_span = parser.expect(TokenKind::CloseCurly)?.span;

    Ok(Stmt {
        kind: StmtKind::Impl(ImplStmt {
            items: methods,
            self_ty,
            interface,
        }),
        attributes,
        span: Span::new(start_span.start(), end_span.end()),
    })
}

pub fn parse_import_stmt(
    parser: &mut Parser,
    attributes: ThinVec<Attribute>,
    modifiers: ThinVec<Modifier>,
) -> Result<Stmt> {
    let (pub_mod,) = get_modifiers!(&parser, modifiers, [Pub]);

    let start_span = parser.expect(TokenKind::Import)?.span;
    let tree = parse_import_tree(parser)?;
    let end_span = parser.expect(TokenKind::Semicolon)?.span;

    let span = Span::new(start_span.start(), end_span.end());

    let visibility = if pub_mod.is_some() {
        Visibility::Public
    } else {
        Visibility::Private
    };

    Ok(Stmt {
        kind: StmtKind::Import(ImportStmt { tree, visibility }),
        attributes,
        span,
    })
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

fn parse_import_tree_list(parser: &mut Parser) -> Result<ThinVec<ImportTree>> {
    let mut items = ThinVec::new();

    parser.expect(TokenKind::OpenCurly)?;

    loop {
        items.push(parse_import_tree(parser)?);

        if parser.current_token().kind == TokenKind::CloseCurly {
            break;
        }

        if parser.current_token().kind == TokenKind::Comma {
            parser.advance();
            if parser.current_token().kind == TokenKind::CloseCurly {
                break;
            }
        } else {
            break;
        }
    }

    parser.expect(TokenKind::CloseCurly)?;

    Ok(items)
}
