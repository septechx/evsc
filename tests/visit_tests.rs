#[cfg(test)]
mod tests {
    use oxic::{
        ast::{
            expressions::*,
            statements::*,
            types::*,
            visit::{VisitAction, Visitable, Visitor},
            *,
        },
        hashmap::FxHashMap,
        lexer::token::{Token, TokenKind},
        span::{ModuleId, Span},
    };

    // Since this is only used for testing, using a string instead of an enum is fine.
    pub struct NodeCounterVisitor {
        stmt_counts: FxHashMap<&'static str, usize>,
        expr_counts: FxHashMap<&'static str, usize>,
        type_counts: FxHashMap<&'static str, usize>,
    }

    impl NodeCounterVisitor {
        pub fn new() -> Self {
            Self {
                stmt_counts: FxHashMap::default(),
                expr_counts: FxHashMap::default(),
                type_counts: FxHashMap::default(),
            }
        }

        pub fn assert_visited(&self, category: &str, name: &str, count: usize) {
            let counts = match category {
                "stmt" => &self.stmt_counts,
                "expr" => &self.expr_counts,
                "type" => &self.type_counts,
                _ => panic!("Invalid category: {}", category),
            };

            let actual = counts.get(name).copied().unwrap_or(0);
            assert_eq!(
                actual, count,
                "Expected {} {} visits, got {}",
                count, name, actual
            );
        }

        pub fn assert_all_visited(&self, expectations: &[(&'static str, &'static str, usize)]) {
            for &(category, name, count) in expectations {
                self.assert_visited(category, name, count);
            }
        }

        #[allow(dead_code)]
        pub fn report(&self) {
            println!("\n=== Node Visit Report ===");
            println!("\nStatements:");
            for (name, count) in &self.stmt_counts {
                println!("  {}: {}", name, count);
            }
            println!("\nExpressions:");
            for (name, count) in &self.expr_counts {
                println!("  {}: {}", name, count);
            }
            println!("\nTypes:");
            for (name, count) in &self.type_counts {
                println!("  {}: {}", name, count);
            }
            println!("========================\n");
        }
    }

    impl Visitor for NodeCounterVisitor {
        fn visit_stmt(&mut self, stmt: &Stmt) -> VisitAction {
            *self.stmt_counts.entry("Stmt").or_insert(0) += 1;

            let kind_name = match &stmt.kind {
                StmtKind::Expression(_) => "ExpressionStmt",
                StmtKind::VarDecl(_) => "VarDeclStmt",
                StmtKind::StructDecl(_) => "StructDeclStmt",
                StmtKind::InterfaceDecl(_) => "InterfaceDeclStmt",
                StmtKind::FnDecl(_) => "FnDeclStmt",
                StmtKind::Return(_) => "ReturnStmt",
                StmtKind::Import(_) => "ImportStmt",
                StmtKind::Impl(_) => "ImplStmt",
            };
            *self.stmt_counts.entry(kind_name).or_insert(0) += 1;

            VisitAction::Continue
        }

        fn visit_expr(&mut self, expr: &Expr) -> VisitAction {
            *self.expr_counts.entry("Expr").or_insert(0) += 1;

            let kind_name = match &expr.kind {
                ExprKind::Literal(l) => {
                    *self.expr_counts.entry("Literal").or_insert(0) += 1;
                    match l {
                        Literal::Integer(_) | Literal::Float(_) => "NumberExpr",
                        Literal::String(_) => "StringExpr",
                        Literal::Char(_) => "CharExpr",
                        Literal::Bool(_) => "BoolExpr",
                    }
                }
                ExprKind::Block(_) => "BlockExpr",
                ExprKind::Symbol(_) => "SymbolExpr",
                ExprKind::Binary(_) => "BinaryExpr",
                ExprKind::Postfix(_) => "PostfixExpr",
                ExprKind::Prefix(_) => "PrefixExpr",
                ExprKind::Assignment(_) => "AssignmentExpr",
                ExprKind::StructInstantiation(_) => "StructInstantiationExpr",
                ExprKind::ArrayLiteral(_) => "ArrayLiteralExpr",
                ExprKind::FunctionCall(_) => "FunctionCallExpr",
                ExprKind::MemberAccess(_) => "MemberAccessExpr",
                ExprKind::Type(_) => "TypeExpr",
                ExprKind::As(_) => "AsExpr",
                ExprKind::TupleLiteral(_) => "TupleLiteralExpr",
            };
            *self.expr_counts.entry(kind_name).or_insert(0) += 1;

            VisitAction::Continue
        }

        fn visit_type(&mut self, ty: &Type) -> VisitAction {
            *self.type_counts.entry("Type").or_insert(0) += 1;

            let kind_name = match &ty.kind {
                TypeKind::Symbol(_) => "SymbolType",
                TypeKind::Pointer(_) => "PointerType",
                TypeKind::Slice(_) => "SliceType",
                TypeKind::FixedArray(_) => "FixedArrayType",
                TypeKind::Function(_) => "FunctionType",
                TypeKind::Tuple(_) => "TupleType",
                TypeKind::Infer => "Infer",
                TypeKind::Never => "Never",
            };
            *self.type_counts.entry(kind_name).or_insert(0) += 1;

            VisitAction::Continue
        }
    }

    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    fn dummy_ident(name: &str) -> Ident {
        Ident {
            value: name.to_string().into_boxed_str(),
            span: dummy_span(),
        }
    }

    fn dummy_token(kind: TokenKind) -> Token {
        Token {
            kind,
            value: "".to_string().into_boxed_str(),
            span: dummy_span(),
            module_id: ModuleId(0),
        }
    }

    fn dummy_type_symbol(name: &str) -> Type {
        Type {
            kind: TypeKind::Symbol(SymbolType {
                name: dummy_ident(name),
            }),
            span: dummy_span(),
        }
    }

    fn dummy_type_infer() -> Type {
        Type {
            kind: TypeKind::Infer,
            span: dummy_span(),
        }
    }

    fn dummy_type_never() -> Type {
        Type {
            kind: TypeKind::Never,
            span: dummy_span(),
        }
    }

    fn dummy_expr_number(value: i32) -> Expr {
        Expr {
            kind: ExprKind::Literal(Literal::Integer(value as i64)),
            span: dummy_span(),
        }
    }

    fn dummy_expr_symbol(name: &str) -> Expr {
        Expr {
            kind: ExprKind::Symbol(SymbolExpr {
                value: dummy_ident(name),
            }),
            span: dummy_span(),
        }
    }

    fn dummy_expr_block(body: Vec<Stmt>) -> Expr {
        Expr {
            kind: ExprKind::Block(BlockExpr {
                body: body.into_boxed_slice(),
            }),
            span: dummy_span(),
        }
    }

    fn dummy_stmt_expr(expr: Expr) -> Stmt {
        Stmt {
            kind: StmtKind::Expression(ExpressionStmt {
                expression: expr,
                has_semicolon: true,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        }
    }

    #[test]
    fn test_number_expr_visited_once() {
        let expr = dummy_expr_number(42);
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_float_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Literal(Literal::Float(3.15)),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "Literal", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_string_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Literal(Literal::String("hello".to_string().into_boxed_str())),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "StringExpr", 1);
    }

    #[test]
    fn test_symbol_expr_visited_once() {
        let expr = dummy_expr_symbol("x");
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "SymbolExpr", 1);
    }

    #[test]
    fn test_bool_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Literal(Literal::Bool(true)),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "Literal", 1);
        visitor.assert_visited("expr", "BoolExpr", 1);
    }

    #[test]
    fn test_char_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Literal(Literal::Char('a')),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "Literal", 1);
        visitor.assert_visited("expr", "CharExpr", 1);
    }

    #[test]
    fn test_binary_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Binary(BinaryExpr {
                left: Box::new(dummy_expr_number(1)),
                operator: dummy_token(TokenKind::Plus),
                right: Box::new(dummy_expr_number(2)),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 3);
        visitor.assert_visited("expr", "BinaryExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 2);
    }

    #[test]
    fn test_postfix_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Postfix(PostfixExpr {
                left: Box::new(dummy_expr_symbol("x")),
                operator: dummy_token(TokenKind::Plus),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "PostfixExpr", 1);
        visitor.assert_visited("expr", "SymbolExpr", 1);
    }

    #[test]
    fn test_prefix_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Prefix(PrefixExpr {
                operator: dummy_token(TokenKind::NotEquals),
                right: Box::new(dummy_expr_symbol("x")),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "PrefixExpr", 1);
        visitor.assert_visited("expr", "SymbolExpr", 1);
    }

    #[test]
    fn test_assignment_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Assignment(AssignmentExpr {
                assigne: Box::new(dummy_expr_symbol("x")),
                operator: dummy_token(TokenKind::Equals),
                value: Box::new(dummy_expr_number(1)),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 3);
        visitor.assert_visited("expr", "AssignmentExpr", 1);
        visitor.assert_visited("expr", "SymbolExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_struct_instantiation_expr_single_prop() {
        let expr = Expr {
            kind: ExprKind::StructInstantiation(StructInstantiationExpr {
                name: dummy_ident("Foo"),
                fields: [(dummy_ident("a"), dummy_expr_number(1))]
                    .into_iter()
                    .collect(),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "StructInstantiationExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_struct_instantiation_expr_multiple_props() {
        let expr = Expr {
            kind: ExprKind::StructInstantiation(StructInstantiationExpr {
                name: dummy_ident("Foo"),
                fields: [
                    (dummy_ident("a"), dummy_expr_number(1)),
                    (dummy_ident("b"), dummy_expr_number(2)),
                    (dummy_ident("c"), dummy_expr_number(3)),
                ]
                .into_iter()
                .collect(),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 4);
        visitor.assert_visited("expr", "StructInstantiationExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 3);
    }

    #[test]
    fn test_array_literal_expr() {
        let expr = Expr {
            kind: ExprKind::ArrayLiteral(ArrayLiteralExpr {
                underlying: dummy_type_symbol("i32"),
                contents: Box::new([
                    dummy_expr_number(1),
                    dummy_expr_number(2),
                    dummy_expr_number(3),
                ]),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 4);
        visitor.assert_visited("expr", "ArrayLiteralExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 3);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_function_call_expr() {
        let expr = Expr {
            kind: ExprKind::FunctionCall(FunctionCallExpr {
                callee: Box::new(dummy_expr_symbol("foo")),
                arguments: Box::new([dummy_expr_number(1), dummy_expr_number(2)]),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 4);
        visitor.assert_visited("expr", "FunctionCallExpr", 1);
        visitor.assert_visited("expr", "SymbolExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 2);
    }

    #[test]
    fn test_member_access_expr() {
        let expr = Expr {
            kind: ExprKind::MemberAccess(MemberAccessExpr {
                base: Box::new(dummy_expr_symbol("obj")),
                member: dummy_ident("field"),
                operator: dummy_token(TokenKind::Dot),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "MemberAccessExpr", 1);
        visitor.assert_visited("expr", "SymbolExpr", 1);
    }

    #[test]
    fn test_type_expr() {
        let expr = Expr {
            kind: ExprKind::Type(TypeExpr {
                underlying: dummy_type_symbol("i32"),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "TypeExpr", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_as_expr() {
        let expr = Expr {
            kind: ExprKind::As(AsExpr {
                expr: Box::new(dummy_expr_number(1)),
                ty: dummy_type_symbol("i32"),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "AsExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_tuple_literal_expr() {
        let expr = Expr {
            kind: ExprKind::TupleLiteral(TupleLiteralExpr {
                elements: Box::new([
                    dummy_expr_number(1),
                    dummy_expr_number(2),
                    dummy_expr_number(3),
                ]),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 4);
        visitor.assert_visited("expr", "TupleLiteralExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 3);
    }

    #[test]
    fn test_block_stmt_empty() {
        let stmt = dummy_expr_block(vec![]);
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 0);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "BlockExpr", 1);
    }

    #[test]
    fn test_block_stmt_with_body() {
        let stmt = dummy_expr_block(vec![dummy_stmt_expr(dummy_expr_number(1))]);
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("expr", "BlockExpr", 1);
        visitor.assert_visited("stmt", "ExpressionStmt", 1);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_expression_stmt() {
        let stmt = dummy_stmt_expr(dummy_expr_number(42));
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "ExpressionStmt", 1);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_var_decl_stmt_with_value() {
        let stmt = Stmt {
            kind: StmtKind::VarDecl(VarDeclStmt {
                variable_name: dummy_ident("x"),
                mutability: Mutability::Mutable,
                visibility: Visibility::Private,
                assigned_value: Some(dummy_expr_number(1)),
                ty: dummy_type_symbol("i32"),
                is_static: false,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "VarDeclStmt", 1);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_var_decl_stmt_no_value() {
        let stmt = Stmt {
            kind: StmtKind::VarDecl(VarDeclStmt {
                variable_name: dummy_ident("x"),
                mutability: Mutability::Mutable,
                visibility: Visibility::Private,
                assigned_value: None,
                ty: dummy_type_symbol("i32"),
                is_static: false,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "VarDeclStmt", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_struct_decl_stmt_empty() {
        let stmt = Stmt {
            kind: StmtKind::StructDecl(StructDeclStmt {
                name: dummy_ident("Foo"),
                fields: Box::new([]),
                methods: Box::new([]),
                visibility: Visibility::Private,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "StructDeclStmt", 1);
    }

    #[test]
    fn test_struct_decl_stmt_with_props() {
        let stmt = Stmt {
            kind: StmtKind::StructDecl(StructDeclStmt {
                name: dummy_ident("Foo"),
                fields: vec![
                    StructField {
                        name: dummy_ident("a"),
                        ty: dummy_type_symbol("i32"),
                        visibility: Visibility::Private,
                    },
                    StructField {
                        name: dummy_ident("b"),
                        ty: dummy_type_symbol("bool"),
                        visibility: Visibility::Private,
                    },
                ]
                .into_boxed_slice(),
                methods: Box::new([]),
                visibility: Visibility::Private,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "StructDeclStmt", 1);
        visitor.assert_visited("type", "Type", 2);
        visitor.assert_visited("type", "SymbolType", 2);
    }

    #[test]
    fn test_struct_decl_stmt_with_methods() {
        let stmt = Stmt {
            kind: StmtKind::StructDecl(StructDeclStmt {
                name: dummy_ident("Foo"),
                fields: Box::new([]),
                methods: vec![StructMethod {
                    is_static: false,
                    visibility: Visibility::Private,
                    fn_decl: FnDeclStmt {
                        name: dummy_ident("bar"),
                        arguments: Box::new([]),
                        body: Some(Expr {
                            kind: ExprKind::Block(BlockExpr { body: Box::new([]) }),
                            span: dummy_span(),
                        }),
                        return_type: dummy_type_never(),
                        visibility: Visibility::Private,
                        is_extern: false,
                    },
                }]
                .into_boxed_slice(),
                visibility: Visibility::Private,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "StructDeclStmt", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "Never", 1);
    }

    #[test]
    fn test_interface_decl_stmt() {
        let stmt = Stmt {
            kind: StmtKind::InterfaceDecl(InterfaceDeclStmt {
                name: dummy_ident("Foo"),
                methods: vec![InterfaceMethod {
                    fn_decl: FnDeclStmt {
                        name: dummy_ident("bar"),
                        arguments: Box::new([]),
                        body: None,
                        return_type: dummy_type_never(),
                        visibility: Visibility::Private,
                        is_extern: false,
                    },
                }]
                .into_boxed_slice(),
                visibility: Visibility::Private,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "InterfaceDeclStmt", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "Never", 1);
    }

    #[test]
    fn test_fn_decl_stmt() {
        let ast = Ast {
            name: "test".into(),
            items: Box::new([Stmt {
                kind: StmtKind::FnDecl(FnDeclStmt {
                    name: dummy_ident("foo"),
                    arguments: vec![
                        FnArgument {
                            name: dummy_ident("a"),
                            ty: dummy_type_symbol("i32"),
                        },
                        FnArgument {
                            name: dummy_ident("b"),
                            ty: dummy_type_symbol("bool"),
                        },
                    ]
                    .into_boxed_slice(),
                    body: Some(Expr {
                        kind: ExprKind::Block(BlockExpr {
                            body: vec![dummy_stmt_expr(dummy_expr_number(1))].into_boxed_slice(),
                        }),
                        span: dummy_span(),
                    }),
                    return_type: dummy_type_symbol("void"),
                    visibility: Visibility::Private,
                    is_extern: false,
                }),
                span: dummy_span(),
                attributes: Box::new([]),
            }]),
        };
        let mut visitor = NodeCounterVisitor::new();
        ast.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 2);
        visitor.assert_visited("stmt", "FnDeclStmt", 1);
        visitor.assert_visited("stmt", "ExpressionStmt", 1);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "BlockExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
        visitor.assert_visited("type", "Type", 3);
        visitor.assert_visited("type", "SymbolType", 3);
    }

    #[test]
    fn test_return_stmt_with_value() {
        let stmt = Stmt {
            kind: StmtKind::Return(ReturnStmt {
                value: Some(dummy_expr_number(1)),
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "ReturnStmt", 1);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_return_stmt_no_value() {
        let stmt = Stmt {
            kind: StmtKind::Return(ReturnStmt { value: None }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "ReturnStmt", 1);
    }

    #[test]
    fn test_import_stmt() {
        let stmt = Stmt {
            kind: StmtKind::Import(ImportStmt {
                tree: ImportTree {
                    prefix: Path {
                        span: dummy_span(),
                        segments: Box::new([dummy_ident("foo")]),
                    },
                    kind: ImportTreeKind::Simple(None),
                    span: dummy_span(),
                },
                visibility: Visibility::Private,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "ImportStmt", 1);
    }

    #[test]
    fn test_symbol_type() {
        let ty = dummy_type_symbol("i32");
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_infer_type() {
        let ty = dummy_type_infer();
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "Infer", 1);
    }

    #[test]
    fn test_never_type() {
        let ty = dummy_type_never();
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "Never", 1);
    }

    #[test]
    fn test_pointer_type() {
        let ty = Type {
            kind: TypeKind::Pointer(PointerType {
                underlying: Box::new(dummy_type_symbol("i32")),
                mutability: Mutability::Constant,
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 2);
        visitor.assert_visited("type", "PointerType", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_slice_type() {
        let ty = Type {
            kind: TypeKind::Slice(SliceType {
                underlying: Box::new(dummy_type_symbol("i32")),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 2);
        visitor.assert_visited("type", "SliceType", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_fixed_array_type() {
        let ty = Type {
            kind: TypeKind::FixedArray(FixedArrayType {
                length: 10,
                underlying: Box::new(dummy_type_symbol("i32")),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 2);
        visitor.assert_visited("type", "FixedArrayType", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_function_type() {
        let ty = Type {
            kind: TypeKind::Function(FunctionType {
                parameters: Box::new([dummy_type_symbol("i32"), dummy_type_symbol("bool")]),
                return_type: Box::new(dummy_type_symbol("void")),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 4);
        visitor.assert_visited("type", "FunctionType", 1);
        visitor.assert_visited("type", "SymbolType", 3);
    }

    #[test]
    fn test_tuple_type() {
        let ty = Type {
            kind: TypeKind::Tuple(TupleType {
                elements: Box::new([dummy_type_symbol("i32"), dummy_type_symbol("bool")]),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 3);
        visitor.assert_visited("type", "TupleType", 1);
        visitor.assert_visited("type", "SymbolType", 2);
    }

    #[test]
    fn test_comprehensive_all_expression_types() {
        let expr = Expr {
            kind: ExprKind::FunctionCall(FunctionCallExpr {
                callee: Box::new(dummy_expr_symbol("foo")),
                arguments: Box::new([
                    dummy_expr_number(1),
                    Expr {
                        kind: ExprKind::StructInstantiation(StructInstantiationExpr {
                            name: dummy_ident("Bar"),
                            fields: [
                                (dummy_ident("x"), dummy_expr_number(2)),
                                (
                                    dummy_ident("y"),
                                    Expr {
                                        kind: ExprKind::Binary(BinaryExpr {
                                            left: Box::new(dummy_expr_number(3)),
                                            operator: dummy_token(TokenKind::Plus),
                                            right: Box::new(dummy_expr_number(4)),
                                        }),
                                        span: dummy_span(),
                                    },
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        }),
                        span: dummy_span(),
                    },
                    Expr {
                        kind: ExprKind::As(AsExpr {
                            expr: Box::new(dummy_expr_symbol("z")),
                            ty: Type {
                                kind: TypeKind::Pointer(PointerType {
                                    underlying: Box::new(dummy_type_symbol("i32")),
                                    mutability: Mutability::Constant,
                                }),
                                span: dummy_span(),
                            },
                        }),
                        span: dummy_span(),
                    },
                ]),
            }),
            span: dummy_span(),
        };

        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);

        visitor.assert_all_visited(&[
            ("expr", "FunctionCallExpr", 1),
            ("expr", "StructInstantiationExpr", 1),
            ("expr", "AsExpr", 1),
            ("expr", "BinaryExpr", 1),
            ("expr", "SymbolExpr", 2),
            ("expr", "NumberExpr", 4),
        ]);
    }

    #[test]
    fn test_comprehensive_all_statement_types() {
        let ast = Ast {
            name: "test".into(),
            items: Box::new([
                Stmt {
                    kind: StmtKind::Import(ImportStmt {
                        tree: ImportTree {
                            prefix: Path {
                                span: dummy_span(),
                                segments: Box::new([dummy_ident("std")]),
                            },
                            kind: ImportTreeKind::Simple(None),
                            span: dummy_span(),
                        },
                        visibility: Visibility::Private,
                    }),
                    span: dummy_span(),
                    attributes: Box::new([]),
                },
                Stmt {
                    kind: StmtKind::StructDecl(StructDeclStmt {
                        name: dummy_ident("Foo"),
                        fields: vec![StructField {
                            name: dummy_ident("x"),
                            ty: dummy_type_symbol("i32"),
                            visibility: Visibility::Private,
                        }]
                        .into_boxed_slice(),
                        methods: vec![StructMethod {
                            is_static: false,
                            visibility: Visibility::Private,
                            fn_decl: FnDeclStmt {
                                name: dummy_ident("method"),
                                arguments: Box::new([]),
                                body: Some(Expr {
                                    kind: ExprKind::Block(BlockExpr {
                                        body: vec![dummy_stmt_expr(dummy_expr_number(1))]
                                            .into_boxed_slice(),
                                    }),
                                    span: dummy_span(),
                                }),
                                return_type: dummy_type_never(),
                                visibility: Visibility::Private,
                                is_extern: false,
                            },
                        }]
                        .into_boxed_slice(),
                        visibility: Visibility::Private,
                    }),
                    span: dummy_span(),
                    attributes: Box::new([]),
                },
                Stmt {
                    kind: StmtKind::InterfaceDecl(InterfaceDeclStmt {
                        name: dummy_ident("Bar"),
                        methods: vec![InterfaceMethod {
                            fn_decl: FnDeclStmt {
                                name: dummy_ident("method"),
                                arguments: Box::new([]),
                                body: Some(Expr {
                                    kind: ExprKind::Block(BlockExpr { body: Box::new([]) }),
                                    span: dummy_span(),
                                }),
                                return_type: dummy_type_never(),
                                visibility: Visibility::Private,
                                is_extern: false,
                            },
                        }]
                        .into_boxed_slice(),
                        visibility: Visibility::Private,
                    }),
                    span: dummy_span(),
                    attributes: Box::new([]),
                },
                Stmt {
                    kind: StmtKind::FnDecl(FnDeclStmt {
                        name: dummy_ident("main"),
                        arguments: Box::new([]),
                        body: Some(Expr {
                            kind: ExprKind::Block(BlockExpr {
                                body: vec![
                                    Stmt {
                                        kind: StmtKind::VarDecl(VarDeclStmt {
                                            variable_name: dummy_ident("a"),
                                            mutability: Mutability::Mutable,
                                            visibility: Visibility::Private,
                                            assigned_value: Some(dummy_expr_number(1)),
                                            ty: dummy_type_infer(),
                                            is_static: false,
                                        }),
                                        span: dummy_span(),
                                        attributes: Box::new([]),
                                    },
                                    Stmt {
                                        kind: StmtKind::Expression(ExpressionStmt {
                                            expression: Expr {
                                                kind: ExprKind::Block(BlockExpr {
                                                    body: vec![Stmt {
                                                        kind: StmtKind::Return(ReturnStmt {
                                                            value: Some(dummy_expr_number(2)),
                                                        }),
                                                        span: dummy_span(),
                                                        attributes: Box::new([]),
                                                    }]
                                                    .into_boxed_slice(),
                                                }),
                                                span: dummy_span(),
                                            },
                                            has_semicolon: true,
                                        }),
                                        span: dummy_span(),
                                        attributes: Box::new([]),
                                    },
                                ]
                                .into_boxed_slice(),
                            }),
                            span: dummy_span(),
                        }),
                        return_type: dummy_type_symbol("isize"),
                        visibility: Visibility::Private,
                        is_extern: false,
                    }),
                    span: dummy_span(),
                    attributes: Box::new([]),
                },
            ]),
        };

        let mut visitor = NodeCounterVisitor::new();
        ast.visit(&mut visitor);

        visitor.assert_all_visited(&[
            ("stmt", "ImportStmt", 1),
            ("stmt", "StructDeclStmt", 1),
            ("stmt", "InterfaceDeclStmt", 1),
            ("stmt", "FnDeclStmt", 1),
            ("stmt", "VarDeclStmt", 1),
            ("expr", "BlockExpr", 4),
            ("stmt", "ReturnStmt", 1),
            ("stmt", "ExpressionStmt", 2),
        ]);
    }

    #[test]
    fn test_comprehensive_all_type_types() {
        let ty = Type {
            kind: TypeKind::Function(FunctionType {
                parameters: Box::new([
                    Type {
                        kind: TypeKind::Pointer(PointerType {
                            underlying: Box::new(dummy_type_symbol("i32")),
                            mutability: Mutability::Mutable,
                        }),
                        span: dummy_span(),
                    },
                    Type {
                        kind: TypeKind::Slice(SliceType {
                            underlying: Box::new(dummy_type_symbol("u8")),
                        }),
                        span: dummy_span(),
                    },
                    Type {
                        kind: TypeKind::FixedArray(FixedArrayType {
                            length: 10,
                            underlying: Box::new(dummy_type_symbol("bool")),
                        }),
                        span: dummy_span(),
                    },
                ]),
                return_type: Box::new(Type {
                    kind: TypeKind::Tuple(TupleType {
                        elements: Box::new([
                            dummy_type_infer(),
                            dummy_type_never(),
                            dummy_type_symbol("void"),
                        ]),
                    }),
                    span: dummy_span(),
                }),
            }),
            span: dummy_span(),
        };

        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);

        visitor.assert_all_visited(&[
            ("type", "Type", 11),
            ("type", "FunctionType", 1),
            ("type", "PointerType", 1),
            ("type", "SliceType", 1),
            ("type", "FixedArrayType", 1),
            ("type", "TupleType", 1),
            ("type", "SymbolType", 4),
            ("type", "Infer", 1),
            ("type", "Never", 1),
        ]);
    }

    #[test]
    fn test_deeply_nested_visits() {
        let expr = Expr {
            kind: ExprKind::Binary(BinaryExpr {
                left: Box::new(Expr {
                    kind: ExprKind::Binary(BinaryExpr {
                        left: Box::new(Expr {
                            kind: ExprKind::Binary(BinaryExpr {
                                left: Box::new(dummy_expr_symbol("a")),
                                operator: dummy_token(TokenKind::Plus),
                                right: Box::new(dummy_expr_symbol("b")),
                            }),
                            span: dummy_span(),
                        }),
                        operator: dummy_token(TokenKind::Star),
                        right: Box::new(Expr {
                            kind: ExprKind::Binary(BinaryExpr {
                                left: Box::new(dummy_expr_symbol("c")),
                                operator: dummy_token(TokenKind::Slash),
                                right: Box::new(dummy_expr_symbol("d")),
                            }),
                            span: dummy_span(),
                        }),
                    }),
                    span: dummy_span(),
                }),
                operator: dummy_token(TokenKind::Dash),
                right: Box::new(dummy_expr_symbol("e")),
            }),
            span: dummy_span(),
        };

        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);

        visitor.assert_all_visited(&[
            ("expr", "Expr", 9),
            ("expr", "BinaryExpr", 4),
            ("expr", "SymbolExpr", 5),
        ]);
    }

    #[test]
    fn test_impl_stmt() {
        let stmt = Stmt {
            kind: StmtKind::Impl(ImplStmt {
                self_ty: dummy_type_symbol("Foo"),
                interface: dummy_ident("Bar"),
                items: vec![InterfaceMethod {
                    fn_decl: FnDeclStmt {
                        name: dummy_ident("bar"),
                        arguments: Box::new([]),
                        body: Some(Expr {
                            kind: ExprKind::Block(BlockExpr { body: Box::new([]) }),
                            span: dummy_span(),
                        }),
                        return_type: dummy_type_symbol("void"),
                        is_extern: false,
                        visibility: Visibility::Private,
                    },
                }]
                .into_boxed_slice(),
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "ImplStmt", 1);
        visitor.assert_visited("type", "SymbolType", 2);
        visitor.assert_visited("expr", "BlockExpr", 1);
    }
}
