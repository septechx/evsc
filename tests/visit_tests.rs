#[cfg(test)]
mod tests {
    use oxic::{
        ast::{
            types::{FixedArrayType, FunctionType, PointerType, SliceType, SymbolType, TupleType},
            visit::{VisitAction, Visitable, Visitor},
            AssocItem, AssocItemKind, Ast, Block, Expr, ExprKind, Fn, Ident, ImportTree,
            ImportTreeKind, Item, ItemKind, Literal, Mutability, Path, Stmt, StmtKind, Type,
            TypeKind, Visibility,
        },
        hashmap::FxHashMap,
        lexer::token::{Token, TokenKind},
        span::{ModuleId, Span},
    };
    use thin_vec::{thin_vec, ThinVec};

    // Since this is only used for testing, using a string instead of an enum is fine.
    pub struct NodeCounterVisitor {
        item_counts: FxHashMap<&'static str, usize>,
        stmt_counts: FxHashMap<&'static str, usize>,
        expr_counts: FxHashMap<&'static str, usize>,
        type_counts: FxHashMap<&'static str, usize>,
    }

    impl NodeCounterVisitor {
        pub fn new() -> Self {
            Self {
                item_counts: FxHashMap::default(),
                stmt_counts: FxHashMap::default(),
                expr_counts: FxHashMap::default(),
                type_counts: FxHashMap::default(),
            }
        }

        pub fn assert_visited(&self, category: &str, name: &str, count: usize) {
            let counts = match category {
                "item" => &self.item_counts,
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
            println!("\nItems:");
            for (name, count) in &self.item_counts {
                println!("  {}: {}", name, count);
            }
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
        fn visit_item(&mut self, item: &Item) -> VisitAction {
            *self.item_counts.entry("Item").or_insert(0) += 1;

            let kind_name = match &item.kind {
                ItemKind::Static { .. } => "StaticItem",
                ItemKind::Struct { .. } => "StructDeclItem",
                ItemKind::Interface { .. } => "InterfaceDeclItem",
                ItemKind::Impl { .. } => "ImplItem",
                ItemKind::Fn(_) => "FnDeclItem",
                ItemKind::Import(_) => "ImportItem",
            };
            *self.item_counts.entry(kind_name).or_insert(0) += 1;

            VisitAction::Continue
        }

        fn visit_stmt(&mut self, stmt: &Stmt) -> VisitAction {
            *self.stmt_counts.entry("Stmt").or_insert(0) += 1;

            let kind_name = match &stmt.kind {
                StmtKind::Expr(_) => "ExprStmt",
                StmtKind::Let { .. } => "LetStmt",
                StmtKind::Semi(_) => "SemiStmt",
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
                ExprKind::If { .. } => "IfExpr",
                ExprKind::While { .. } => "WhileExpr",
                ExprKind::Loop(_) => "LoopExpr",
                ExprKind::Symbol(_) => "SymbolExpr",
                ExprKind::Binary { .. } => "BinaryExpr",
                ExprKind::Postfix { .. } => "PostfixExpr",
                ExprKind::Prefix { .. } => "PrefixExpr",
                ExprKind::Assignment { .. } => "AssignmentExpr",
                ExprKind::StructInstantiation { .. } => "StructInstantiationExpr",
                ExprKind::ArrayLiteral { .. } => "ArrayLiteralExpr",
                ExprKind::FunctionCall { .. } => "FunctionCallExpr",
                ExprKind::MemberAccess { .. } => "MemberAccessExpr",
                ExprKind::Type(_) => "TypeExpr",
                ExprKind::As { .. } => "AsExpr",
                ExprKind::TupleLiteral { .. } => "TupleLiteralExpr",
                ExprKind::Break(_) => "BreakExpr",
                ExprKind::Return(_) => "ReturnExpr",
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
            kind: ExprKind::Symbol(dummy_ident(name)),
            span: dummy_span(),
        }
    }

    fn dummy_expr_block(body: ThinVec<Stmt>) -> Expr {
        Expr {
            kind: ExprKind::Block(Block { stmts: body }),
            span: dummy_span(),
        }
    }

    fn dummy_stmt_expr(expr: Expr) -> Stmt {
        Stmt {
            kind: StmtKind::Expr(expr),
            span: dummy_span(),
        }
    }

    fn dummy_fn_body() -> Option<Block> {
        Some(Block {
            stmts: ThinVec::new(),
        })
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
            kind: ExprKind::Binary {
                left: Box::new(dummy_expr_number(1)),
                operator: dummy_token(TokenKind::Plus),
                right: Box::new(dummy_expr_number(2)),
            },
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
            kind: ExprKind::Postfix {
                left: Box::new(dummy_expr_symbol("x")),
                operator: dummy_token(TokenKind::Plus),
            },
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
            kind: ExprKind::Prefix {
                operator: dummy_token(TokenKind::NotEquals),
                right: Box::new(dummy_expr_symbol("x")),
            },
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
            kind: ExprKind::Assignment {
                assignee: Box::new(dummy_expr_symbol("x")),
                operator: dummy_token(TokenKind::Equals),
                value: Box::new(dummy_expr_number(1)),
            },
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
            kind: ExprKind::StructInstantiation {
                name: dummy_ident("Foo"),
                fields: thin_vec![(dummy_ident("a"), dummy_expr_number(1))],
            },
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
            kind: ExprKind::StructInstantiation {
                name: dummy_ident("Foo"),
                fields: thin_vec![
                    (dummy_ident("a"), dummy_expr_number(1)),
                    (dummy_ident("b"), dummy_expr_number(2)),
                    (dummy_ident("c"), dummy_expr_number(3)),
                ],
            },
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
            kind: ExprKind::ArrayLiteral {
                underlying: dummy_type_symbol("i32"),
                contents: thin_vec![
                    dummy_expr_number(1),
                    dummy_expr_number(2),
                    dummy_expr_number(3),
                ],
            },
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
            kind: ExprKind::FunctionCall {
                callee: Box::new(dummy_expr_symbol("foo")),
                parameters: thin_vec![dummy_expr_number(1), dummy_expr_number(2)],
            },
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
            kind: ExprKind::MemberAccess {
                base: Box::new(dummy_expr_symbol("obj")),
                member: dummy_ident("field"),
                operator: dummy_token(TokenKind::Dot),
            },
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
            kind: ExprKind::Type(dummy_type_symbol("i32")),
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
            kind: ExprKind::As {
                expr: Box::new(dummy_expr_number(1)),
                ty: dummy_type_symbol("i32"),
            },
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
            kind: ExprKind::TupleLiteral {
                elements: thin_vec![
                    dummy_expr_number(1),
                    dummy_expr_number(2),
                    dummy_expr_number(3),
                ],
            },
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 4);
        visitor.assert_visited("expr", "TupleLiteralExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 3);
    }

    #[test]
    fn test_break_expr_visited_once() {
        let expr = Expr {
            kind: ExprKind::Break(None),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "BreakExpr", 1);
    }

    #[test]
    fn test_break_expr_with_value_visited() {
        let expr = Expr {
            kind: ExprKind::Break(Some(Box::new(dummy_expr_number(42)))),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "BreakExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_block_stmt_empty() {
        let stmt = dummy_expr_block(thin_vec![]);
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 0);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "BlockExpr", 1);
    }

    #[test]
    fn test_block_stmt_with_body() {
        let stmt = dummy_expr_block(thin_vec![dummy_stmt_expr(dummy_expr_number(1))]);
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("expr", "BlockExpr", 1);
        visitor.assert_visited("stmt", "ExprStmt", 1);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_expression_stmt() {
        let stmt = dummy_stmt_expr(dummy_expr_number(42));
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "ExprStmt", 1);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_let_stmt_with_value() {
        let stmt = Stmt {
            kind: StmtKind::Let {
                name: dummy_ident("x"),
                value: Some(dummy_expr_number(1)),
                ty: dummy_type_symbol("i32"),
                mutability: Mutability::Mutable,
            },
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "LetStmt", 1);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_let_stmt_no_value() {
        let stmt = Stmt {
            kind: StmtKind::Let {
                name: dummy_ident("x"),
                value: None,
                ty: dummy_type_symbol("i32"),
                mutability: Mutability::Mutable,
            },
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "LetStmt", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_struct_decl_item_empty() {
        let item = Item {
            kind: ItemKind::Struct {
                name: dummy_ident("Foo"),
                fields: ThinVec::new(),
                items: ThinVec::new(),
            },
            span: dummy_span(),
            attributes: ThinVec::new(),
            visibility: Visibility::Private,
        };
        let mut visitor = NodeCounterVisitor::new();
        item.visit(&mut visitor);
        visitor.assert_visited("item", "Item", 1);
        visitor.assert_visited("item", "StructDeclItem", 1);
    }

    #[test]
    fn test_struct_decl_item_with_props() {
        let item = Item {
            kind: ItemKind::Struct {
                name: dummy_ident("Foo"),
                fields: thin_vec![
                    (
                        dummy_ident("a"),
                        dummy_type_symbol("i32"),
                        Visibility::Private,
                    ),
                    (
                        dummy_ident("b"),
                        dummy_type_symbol("bool"),
                        Visibility::Private,
                    ),
                ],
                items: ThinVec::new(),
            },
            span: dummy_span(),
            attributes: ThinVec::new(),
            visibility: Visibility::Private,
        };
        let mut visitor = NodeCounterVisitor::new();
        item.visit(&mut visitor);
        visitor.assert_visited("item", "Item", 1);
        visitor.assert_visited("item", "StructDeclItem", 1);
        visitor.assert_visited("type", "Type", 2);
        visitor.assert_visited("type", "SymbolType", 2);
    }

    #[test]
    fn test_struct_decl_item_with_methods() {
        let item = Item {
            kind: ItemKind::Struct {
                name: dummy_ident("Foo"),
                fields: ThinVec::new(),
                items: thin_vec![AssocItem {
                    kind: AssocItemKind::Fn(Fn {
                        name: dummy_ident("bar"),
                        parameters: ThinVec::new(),
                        body: dummy_fn_body(),
                        return_type: dummy_type_never(),
                        is_extern: false,
                    }),
                    is_static: false,
                    visibility: Visibility::Private,
                }],
            },
            span: dummy_span(),
            attributes: ThinVec::new(),
            visibility: Visibility::Private,
        };
        let mut visitor = NodeCounterVisitor::new();
        item.visit(&mut visitor);
        visitor.assert_visited("item", "Item", 1);
        visitor.assert_visited("item", "StructDeclItem", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "Never", 1);
    }

    #[test]
    fn test_interface_decl_item() {
        let item = Item {
            kind: ItemKind::Interface {
                name: dummy_ident("Foo"),
                items: thin_vec![AssocItem {
                    kind: AssocItemKind::Fn(Fn {
                        name: dummy_ident("bar"),
                        parameters: ThinVec::new(),
                        body: None,
                        return_type: dummy_type_never(),
                        is_extern: false,
                    }),
                    is_static: false,
                    visibility: Visibility::Private,
                }],
            },
            span: dummy_span(),
            attributes: ThinVec::new(),
            visibility: Visibility::Private,
        };
        let mut visitor = NodeCounterVisitor::new();
        item.visit(&mut visitor);
        visitor.assert_visited("item", "Item", 1);
        visitor.assert_visited("item", "InterfaceDeclItem", 1);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "Never", 1);
    }

    #[test]
    fn test_fn_decl_item() {
        let ast = Ast {
            name: "test".into(),
            items: thin_vec![Item {
                kind: ItemKind::Fn(Fn {
                    name: dummy_ident("foo"),
                    parameters: thin_vec![
                        (dummy_ident("a"), dummy_type_symbol("i32"),),
                        (dummy_ident("b"), dummy_type_symbol("bool"),),
                    ],
                    body: Some(Block {
                        stmts: thin_vec![dummy_stmt_expr(dummy_expr_number(1))],
                    }),
                    return_type: dummy_type_symbol("void"),
                    is_extern: false,
                }),
                span: dummy_span(),
                attributes: ThinVec::new(),
                visibility: Visibility::Private,
            }],
        };
        let mut visitor = NodeCounterVisitor::new();
        ast.visit(&mut visitor);
        visitor.assert_visited("item", "Item", 1);
        visitor.assert_visited("item", "FnDeclItem", 1);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "ExprStmt", 1);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
        visitor.assert_visited("type", "Type", 3);
        visitor.assert_visited("type", "SymbolType", 3);
    }

    #[test]
    fn test_return_with_value() {
        let expr = Expr {
            kind: ExprKind::Return(Some(Box::new(dummy_expr_number(1)))),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 2);
        visitor.assert_visited("expr", "ReturnExpr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_return_no_value() {
        let expr = Expr {
            kind: ExprKind::Return(None),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "ReturnExpr", 1);
    }

    #[test]
    fn test_import_item() {
        let item = Item {
            kind: ItemKind::Import(ImportTree {
                prefix: Path {
                    span: dummy_span(),
                    segments: thin_vec![dummy_ident("foo")],
                },
                kind: ImportTreeKind::Simple(None),
                span: dummy_span(),
            }),
            span: dummy_span(),
            attributes: ThinVec::new(),
            visibility: Visibility::Private,
        };
        let mut visitor = NodeCounterVisitor::new();
        item.visit(&mut visitor);
        visitor.assert_visited("item", "Item", 1);
        visitor.assert_visited("item", "ImportItem", 1);
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
                parameters: thin_vec![dummy_type_symbol("i32"), dummy_type_symbol("bool")],
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
                elements: thin_vec![dummy_type_symbol("i32"), dummy_type_symbol("bool")],
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
            kind: ExprKind::FunctionCall {
                callee: Box::new(dummy_expr_symbol("foo")),
                parameters: thin_vec![
                    dummy_expr_number(1),
                    Expr {
                        kind: ExprKind::StructInstantiation {
                            name: dummy_ident("Bar"),
                            fields: thin_vec![
                                (dummy_ident("x"), dummy_expr_number(2)),
                                (
                                    dummy_ident("y"),
                                    Expr {
                                        kind: ExprKind::Binary {
                                            left: Box::new(dummy_expr_number(3)),
                                            operator: dummy_token(TokenKind::Plus),
                                            right: Box::new(dummy_expr_number(4)),
                                        },
                                        span: dummy_span(),
                                    },
                                ),
                            ],
                        },
                        span: dummy_span(),
                    },
                    Expr {
                        kind: ExprKind::As {
                            expr: Box::new(dummy_expr_symbol("z")),
                            ty: Type {
                                kind: TypeKind::Pointer(PointerType {
                                    underlying: Box::new(dummy_type_symbol("i32")),
                                    mutability: Mutability::Constant,
                                }),
                                span: dummy_span(),
                            },
                        },
                        span: dummy_span(),
                    },
                ],
            },
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
    fn test_comprehensive_all_item_and_statement_types() {
        let ast = Ast {
            name: "test".into(),
            items: thin_vec![
                Item {
                    kind: ItemKind::Import(ImportTree {
                        prefix: Path {
                            span: dummy_span(),
                            segments: thin_vec![dummy_ident("std")],
                        },
                        kind: ImportTreeKind::Simple(None),
                        span: dummy_span(),
                    },),
                    span: dummy_span(),
                    attributes: ThinVec::new(),
                    visibility: Visibility::Private,
                },
                Item {
                    kind: ItemKind::Struct {
                        name: dummy_ident("Foo"),
                        fields: thin_vec![(
                            dummy_ident("x"),
                            dummy_type_symbol("i32"),
                            Visibility::Private,
                        )],
                        items: thin_vec![AssocItem {
                            kind: AssocItemKind::Fn(Fn {
                                name: dummy_ident("method"),
                                parameters: ThinVec::new(),
                                body: Some(Block {
                                    stmts: thin_vec![dummy_stmt_expr(dummy_expr_number(1))],
                                }),
                                return_type: dummy_type_never(),
                                is_extern: false,
                            }),
                            is_static: false,
                            visibility: Visibility::Private,
                        }],
                    },
                    span: dummy_span(),
                    attributes: ThinVec::new(),
                    visibility: Visibility::Private,
                },
                Item {
                    kind: ItemKind::Interface {
                        name: dummy_ident("Bar"),
                        items: thin_vec![AssocItem {
                            kind: AssocItemKind::Fn(Fn {
                                name: dummy_ident("method"),
                                parameters: ThinVec::new(),
                                body: dummy_fn_body(),
                                return_type: dummy_type_never(),
                                is_extern: false,
                            }),
                            is_static: false,
                            visibility: Visibility::Private,
                        }],
                    },
                    span: dummy_span(),
                    attributes: ThinVec::new(),
                    visibility: Visibility::Private,
                },
                Item {
                    kind: ItemKind::Fn(Fn {
                        name: dummy_ident("main"),
                        parameters: ThinVec::new(),
                        body: Some(Block {
                            stmts: thin_vec![
                                Stmt {
                                    kind: StmtKind::Let {
                                        name: dummy_ident("a"),
                                        value: Some(dummy_expr_number(1)),
                                        ty: dummy_type_infer(),
                                        mutability: Mutability::Mutable,
                                    },
                                    span: dummy_span(),
                                },
                                Stmt {
                                    kind: StmtKind::Expr(Expr {
                                        kind: ExprKind::Block(Block {
                                            stmts: thin_vec![Stmt {
                                                kind: StmtKind::Semi(Expr {
                                                    kind: ExprKind::Return(Some(Box::new(
                                                        dummy_expr_number(2)
                                                    ))),
                                                    span: dummy_span()
                                                }),
                                                span: dummy_span(),
                                            }],
                                        }),
                                        span: dummy_span(),
                                    }),
                                    span: dummy_span(),
                                },
                            ],
                        }),
                        return_type: dummy_type_symbol("isize"),
                        is_extern: false,
                    }),
                    span: dummy_span(),
                    attributes: ThinVec::new(),
                    visibility: Visibility::Private,
                },
            ],
        };

        let mut visitor = NodeCounterVisitor::new();
        ast.visit(&mut visitor);

        visitor.assert_all_visited(&[
            ("item", "ImportItem", 1),
            ("item", "StructDeclItem", 1),
            ("item", "InterfaceDeclItem", 1),
            ("item", "FnDeclItem", 1),
            ("stmt", "LetStmt", 1),
            ("expr", "BlockExpr", 1),
            ("expr", "ReturnExpr", 1),
            ("stmt", "SemiStmt", 1),
        ]);
    }

    #[test]
    fn test_comprehensive_all_type_types() {
        let ty = Type {
            kind: TypeKind::Function(FunctionType {
                parameters: thin_vec![
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
                ],
                return_type: Box::new(Type {
                    kind: TypeKind::Tuple(TupleType {
                        elements: thin_vec![
                            dummy_type_infer(),
                            dummy_type_never(),
                            dummy_type_symbol("void"),
                        ],
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
            kind: ExprKind::Binary {
                left: Box::new(Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(Expr {
                            kind: ExprKind::Binary {
                                left: Box::new(dummy_expr_symbol("a")),
                                operator: dummy_token(TokenKind::Plus),
                                right: Box::new(dummy_expr_symbol("b")),
                            },
                            span: dummy_span(),
                        }),
                        operator: dummy_token(TokenKind::Star),
                        right: Box::new(Expr {
                            kind: ExprKind::Binary {
                                left: Box::new(dummy_expr_symbol("c")),
                                operator: dummy_token(TokenKind::Slash),
                                right: Box::new(dummy_expr_symbol("d")),
                            },
                            span: dummy_span(),
                        }),
                    },
                    span: dummy_span(),
                }),
                operator: dummy_token(TokenKind::Dash),
                right: Box::new(dummy_expr_symbol("e")),
            },
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
    fn test_impl_item() {
        let item = Item {
            kind: ItemKind::Impl {
                self_ty: dummy_type_symbol("Foo"),
                interface: dummy_ident("Bar"),
                items: thin_vec![AssocItem {
                    kind: AssocItemKind::Fn(Fn {
                        name: dummy_ident("bar"),
                        parameters: ThinVec::new(),
                        body: dummy_fn_body(),
                        return_type: dummy_type_symbol("void"),
                        is_extern: false,
                    }),
                    is_static: false,
                    visibility: Visibility::Private,
                }],
            },
            span: dummy_span(),
            attributes: ThinVec::new(),
            visibility: Visibility::Private,
        };
        let mut visitor = NodeCounterVisitor::new();
        item.visit(&mut visitor);
        visitor.assert_visited("item", "Item", 1);
        visitor.assert_visited("item", "ImplItem", 1);
        visitor.assert_visited("type", "SymbolType", 2);
    }
}
