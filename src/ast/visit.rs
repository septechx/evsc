use std::{collections::HashMap, hash::Hash};

use crate::ast::{
    Ast, Expr, ExprKind, Stmt, StmtKind, Type, TypeKind, expressions::*, statements::*, types::*,
};

pub trait Visitor {
    fn visit_stmt(&mut self, stmt: &mut Stmt);
    fn visit_expr(&mut self, expr: &mut Expr);
    fn visit_type(&mut self, ty: &mut Type);
}

pub trait Visitable {
    fn visit(&mut self, visitor: &mut impl Visitor);
}

impl<T: Visitable> Visitable for Box<T> {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.as_mut().visit(visitor);
    }
}

impl<T: Visitable> Visitable for Option<T> {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        if let Some(inner) = self {
            inner.visit(visitor);
        }
    }
}

impl<T: Visitable> Visitable for Vec<T> {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for inner in self {
            inner.visit(visitor);
        }
    }
}

impl<T: Visitable> Visitable for Box<[T]> {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for inner in self.iter_mut() {
            inner.visit(visitor);
        }
    }
}

impl<K: Eq + Hash, V: Visitable> Visitable for HashMap<K, V> {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for (_, value) in self.iter_mut() {
            value.visit(visitor);
        }
    }
}

impl Visitable for Ast {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for stmt in &mut self.0 {
            stmt.visit(visitor);
        }
    }
}

impl Visitable for Stmt {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_stmt(self);

        match &mut self.kind {
            StmtKind::Expression(expr_stmt) => expr_stmt.visit(visitor),
            StmtKind::VarDecl(var) => var.visit(visitor),
            StmtKind::StructDecl(s) => s.visit(visitor),
            StmtKind::InterfaceDecl(i) => i.visit(visitor),
            StmtKind::FnDecl(f) => f.visit(visitor),
            StmtKind::Return(r) => r.visit(visitor),
            StmtKind::Import(i) => i.visit(visitor),
        }
    }
}

impl Visitable for ImportStmt {
    fn visit(&mut self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}

impl Visitable for BlockExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.body.visit(visitor);
    }
}

impl Visitable for ExpressionStmt {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.expression.visit(visitor);
    }
}

impl Visitable for VarDeclStmt {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        if let Some(val) = &mut self.assigned_value {
            val.visit(visitor);
        }
        self.type_.visit(visitor);
    }
}

impl Visitable for StructProperty {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.type_.visit(visitor);
    }
}

impl Visitable for StructMethod {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.fn_decl.visit(visitor);
    }
}

impl Visitable for StructDeclStmt {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for p in &mut self.properties {
            p.visit(visitor);
        }
        for m in &mut self.methods {
            m.visit(visitor);
        }
    }
}

impl Visitable for InterfaceMethod {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.fn_decl.visit(visitor);
    }
}

impl Visitable for InterfaceDeclStmt {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for m in &mut self.methods {
            m.visit(visitor);
        }
    }
}

impl Visitable for FnArgument {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.type_.visit(visitor);
    }
}

impl Visitable for FnDeclStmt {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for arg in &mut self.arguments {
            arg.visit(visitor);
        }
        if let Some(body) = &mut self.body {
            body.visit(visitor);
        }
        self.return_type.visit(visitor);
    }
}

impl Visitable for ReturnStmt {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        if let Some(v) = &mut self.value {
            v.visit(visitor);
        }
    }
}

impl Visitable for Expr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_expr(self);

        match &mut self.kind {
            ExprKind::Block(block) => block.visit(visitor),
            ExprKind::Number(n) => n.visit(visitor),
            ExprKind::String(s) => s.visit(visitor),
            ExprKind::Symbol(s) => s.visit(visitor),
            ExprKind::Binary(b) => b.visit(visitor),
            ExprKind::Postfix(p) => p.visit(visitor),
            ExprKind::Prefix(p) => p.visit(visitor),
            ExprKind::Assignment(a) => a.visit(visitor),
            ExprKind::StructInstantiation(s) => s.visit(visitor),
            ExprKind::ArrayLiteral(a) => a.visit(visitor),
            ExprKind::FunctionCall(f) => f.visit(visitor),
            ExprKind::MemberAccess(m) => m.visit(visitor),
            ExprKind::Type(t) => t.visit(visitor),
            ExprKind::As(a) => a.visit(visitor),
            ExprKind::TupleLiteral(t) => t.visit(visitor),
        }
    }
}

impl Visitable for NumberExpr {
    fn visit(&mut self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}
impl Visitable for StringExpr {
    fn visit(&mut self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}
impl Visitable for SymbolExpr {
    fn visit(&mut self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}

impl Visitable for BinaryExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.left.visit(visitor);
        self.right.visit(visitor);
    }
}
impl Visitable for PostfixExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.left.visit(visitor);
    }
}
impl Visitable for PrefixExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.right.visit(visitor);
    }
}
impl Visitable for AssignmentExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.assigne.visit(visitor);
        self.value.visit(visitor);
    }
}
impl Visitable for StructInstantiationExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.properties.visit(visitor);
    }
}
impl Visitable for ArrayLiteralExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
        self.contents.visit(visitor);
    }
}
impl Visitable for FunctionCallExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.callee.visit(visitor);
        self.arguments.visit(visitor);
    }
}
impl Visitable for MemberAccessExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.base.visit(visitor);
    }
}
impl Visitable for TypeExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for AsExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.expr.visit(visitor);
        self.ty.visit(visitor);
    }
}

impl Visitable for TupleLiteralExpr {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for element in &mut self.elements {
            element.visit(visitor);
        }
    }
}

impl Visitable for Type {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_type(self);
        match &mut self.kind {
            TypeKind::Symbol(_) => {}
            TypeKind::Pointer(p) => p.underlying.visit(visitor),
            TypeKind::Slice(s) => s.underlying.visit(visitor),
            TypeKind::FixedArray(f) => {
                f.underlying.visit(visitor);
            }
            TypeKind::Mut(m) => m.underlying.visit(visitor),
            TypeKind::Function(ft) => {
                ft.parameters.visit(visitor);
                ft.return_type.visit(visitor);
            }
            TypeKind::Tuple(t) => {
                t.elements.visit(visitor);
            }
            TypeKind::Infer => {
                // Leaf
            }
            TypeKind::Never => {
                // Leaf
            }
        }
    }
}

impl Visitable for SymbolType {
    fn visit(&mut self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}
impl Visitable for PointerType {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for SliceType {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for FixedArrayType {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for MutType {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for FunctionType {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for p in &mut self.parameters {
            p.visit(visitor);
        }
        self.return_type.visit(visitor);
    }
}

impl Visitable for TupleType {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for element in &mut self.elements {
            element.visit(visitor);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Ident, ImportTree, ImportTreeKind, Path},
        span::{ModuleId, Span},
    };
    use std::collections::HashMap;

    pub struct NodeCounterVisitor {
        stmt_counts: HashMap<&'static str, usize>,
        expr_counts: HashMap<&'static str, usize>,
        type_counts: HashMap<&'static str, usize>,
    }

    impl NodeCounterVisitor {
        pub fn new() -> Self {
            Self {
                stmt_counts: HashMap::new(),
                expr_counts: HashMap::new(),
                type_counts: HashMap::new(),
            }
        }

        pub fn assert_visited(&self, category: &str, name: &str, count: usize) {
            // Since this is only used for testing, using a string instead of an enum is fine.
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
        fn visit_stmt(&mut self, stmt: &mut Stmt) {
            let kind_name = match &stmt.kind {
                StmtKind::Expression(_) => "Stmt",
                StmtKind::VarDecl(_) => "Stmt",
                StmtKind::StructDecl(_) => "Stmt",
                StmtKind::InterfaceDecl(_) => "Stmt",
                StmtKind::FnDecl(_) => "Stmt",
                StmtKind::Return(_) => "Stmt",
                StmtKind::Import(_) => "Stmt",
            };
            *self.stmt_counts.entry(kind_name).or_insert(0) += 1;

            let kind_name = match &stmt.kind {
                StmtKind::Expression(_) => "ExpressionStmt",
                StmtKind::VarDecl(_) => "VarDeclStmt",
                StmtKind::StructDecl(_) => "StructDeclStmt",
                StmtKind::InterfaceDecl(_) => "InterfaceDeclStmt",
                StmtKind::FnDecl(_) => "FnDeclStmt",
                StmtKind::Return(_) => "ReturnStmt",
                StmtKind::Import(_) => "ImportStmt",
            };
            *self.stmt_counts.entry(kind_name).or_insert(0) += 1;
        }

        fn visit_expr(&mut self, expr: &mut Expr) {
            *self.expr_counts.entry("Expr").or_insert(0) += 1;

            let kind_name = match &expr.kind {
                ExprKind::Block(_) => "BlockExpr",
                ExprKind::Number(_) => "NumberExpr",
                ExprKind::String(_) => "StringExpr",
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
        }

        fn visit_type(&mut self, ty: &mut Type) {
            *self.type_counts.entry("Type").or_insert(0) += 1;

            let kind_name = match &ty.kind {
                TypeKind::Symbol(_) => "SymbolType",
                TypeKind::Pointer(_) => "PointerType",
                TypeKind::Slice(_) => "SliceType",
                TypeKind::FixedArray(_) => "FixedArrayType",
                TypeKind::Mut(_) => "MutType",
                TypeKind::Function(_) => "FunctionType",
                TypeKind::Tuple(_) => "TupleType",
                TypeKind::Infer => "Infer",
                TypeKind::Never => "Never",
            };
            *self.type_counts.entry(kind_name).or_insert(0) += 1;
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

    fn dummy_token(kind: crate::lexer::token::TokenKind) -> crate::lexer::token::Token {
        crate::lexer::token::Token {
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
            kind: ExprKind::Number(NumberExpr { value }),
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
        let mut expr = dummy_expr_number(42);
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_string_expr_visited_once() {
        let mut expr = Expr {
            kind: ExprKind::String(StringExpr {
                value: "hello".to_string().into_boxed_str(),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "StringExpr", 1);
    }

    #[test]
    fn test_symbol_expr_visited_once() {
        let mut expr = dummy_expr_symbol("x");
        let mut visitor = NodeCounterVisitor::new();
        expr.visit(&mut visitor);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "SymbolExpr", 1);
    }

    #[test]
    fn test_binary_expr_visited_once() {
        let mut expr = Expr {
            kind: ExprKind::Binary(BinaryExpr {
                left: Box::new(dummy_expr_number(1)),
                operator: dummy_token(crate::lexer::token::TokenKind::Plus),
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
        let mut expr = Expr {
            kind: ExprKind::Postfix(PostfixExpr {
                left: Box::new(dummy_expr_symbol("x")),
                operator: dummy_token(crate::lexer::token::TokenKind::Plus),
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
        let mut expr = Expr {
            kind: ExprKind::Prefix(PrefixExpr {
                operator: dummy_token(crate::lexer::token::TokenKind::NotEquals),
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
        let mut expr = Expr {
            kind: ExprKind::Assignment(AssignmentExpr {
                assigne: Box::new(dummy_expr_symbol("x")),
                operator: dummy_token(crate::lexer::token::TokenKind::Equals),
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
        let mut expr = Expr {
            kind: ExprKind::StructInstantiation(StructInstantiationExpr {
                name: dummy_ident("Foo"),
                properties: HashMap::from([(dummy_ident("a"), dummy_expr_number(1))]),
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
        let mut expr = Expr {
            kind: ExprKind::StructInstantiation(StructInstantiationExpr {
                name: dummy_ident("Foo"),
                properties: HashMap::from([
                    (dummy_ident("a"), dummy_expr_number(1)),
                    (dummy_ident("b"), dummy_expr_number(2)),
                    (dummy_ident("c"), dummy_expr_number(3)),
                ]),
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
        let mut expr = Expr {
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
        let mut expr = Expr {
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
        let mut expr = Expr {
            kind: ExprKind::MemberAccess(MemberAccessExpr {
                base: Box::new(dummy_expr_symbol("obj")),
                member: dummy_ident("field"),
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
        let mut expr = Expr {
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
        let mut expr = Expr {
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
        let mut expr = Expr {
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
        let mut stmt = dummy_expr_block(vec![]);
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 0);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "BlockExpr", 1);
    }

    #[test]
    fn test_block_stmt_with_body() {
        let mut stmt = dummy_expr_block(vec![dummy_stmt_expr(dummy_expr_number(1))]);
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
        let mut stmt = dummy_stmt_expr(dummy_expr_number(42));
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
        visitor.assert_visited("stmt", "Stmt", 1);
        visitor.assert_visited("stmt", "ExpressionStmt", 1);
        visitor.assert_visited("expr", "Expr", 1);
        visitor.assert_visited("expr", "NumberExpr", 1);
    }

    #[test]
    fn test_var_decl_stmt_with_value() {
        let mut stmt = Stmt {
            kind: StmtKind::VarDecl(VarDeclStmt {
                variable_name: dummy_ident("x"),
                is_constant: false,
                is_public: false,
                assigned_value: Some(dummy_expr_number(1)),
                type_: dummy_type_symbol("i32"),
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
        let mut stmt = Stmt {
            kind: StmtKind::VarDecl(VarDeclStmt {
                variable_name: dummy_ident("x"),
                is_constant: false,
                is_public: false,
                assigned_value: None,
                type_: dummy_type_symbol("i32"),
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
        let mut stmt = Stmt {
            kind: StmtKind::StructDecl(StructDeclStmt {
                name: dummy_ident("Foo"),
                properties: Box::new([]),
                methods: Box::new([]),
                is_public: false,
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
        let mut stmt = Stmt {
            kind: StmtKind::StructDecl(StructDeclStmt {
                name: dummy_ident("Foo"),
                properties: vec![
                    StructProperty {
                        name: dummy_ident("a"),
                        type_: dummy_type_symbol("i32"),
                        is_public: false,
                    },
                    StructProperty {
                        name: dummy_ident("b"),
                        type_: dummy_type_symbol("bool"),
                        is_public: false,
                    },
                ]
                .into_boxed_slice(),
                methods: Box::new([]),
                is_public: false,
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
        let mut stmt = Stmt {
            kind: StmtKind::StructDecl(StructDeclStmt {
                name: dummy_ident("Foo"),
                properties: Box::new([]),
                methods: vec![StructMethod {
                    is_static: false,
                    is_public: false,
                    fn_decl: FnDeclStmt {
                        name: dummy_ident("bar"),
                        arguments: Box::new([]),
                        body: Some(Expr {
                            kind: ExprKind::Block(BlockExpr { body: Box::new([]) }),
                            span: dummy_span(),
                        }),
                        return_type: dummy_type_never(),
                        is_public: false,
                        is_extern: false,
                    },
                }]
                .into_boxed_slice(),
                is_public: false,
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
        let mut stmt = Stmt {
            kind: StmtKind::InterfaceDecl(InterfaceDeclStmt {
                name: dummy_ident("Foo"),
                methods: vec![InterfaceMethod {
                    fn_decl: FnDeclStmt {
                        name: dummy_ident("bar"),
                        arguments: Box::new([]),
                        body: None,
                        return_type: dummy_type_never(),
                        is_public: false,
                        is_extern: false,
                    },
                }]
                .into_boxed_slice(),
                is_public: false,
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
        let mut stmt = Stmt {
            kind: StmtKind::FnDecl(FnDeclStmt {
                name: dummy_ident("foo"),
                arguments: vec![
                    FnArgument {
                        name: dummy_ident("a"),
                        type_: dummy_type_symbol("i32"),
                    },
                    FnArgument {
                        name: dummy_ident("b"),
                        type_: dummy_type_symbol("bool"),
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
                is_public: false,
                is_extern: false,
            }),
            span: dummy_span(),
            attributes: Box::new([]),
        };
        let mut visitor = NodeCounterVisitor::new();
        stmt.visit(&mut visitor);
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
        let mut stmt = Stmt {
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
        let mut stmt = Stmt {
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
        let mut stmt = Stmt {
            kind: StmtKind::Import(ImportStmt {
                tree: ImportTree {
                    prefix: Path {
                        span: dummy_span(),
                        segments: Box::new([dummy_ident("foo")]),
                    },
                    kind: ImportTreeKind::Simple(None),
                    span: dummy_span(),
                },
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
        let mut ty = dummy_type_symbol("i32");
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_infer_type() {
        let mut ty = dummy_type_infer();
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "Infer", 1);
    }

    #[test]
    fn test_never_type() {
        let mut ty = dummy_type_never();
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 1);
        visitor.assert_visited("type", "Never", 1);
    }

    #[test]
    fn test_pointer_type() {
        let mut ty = Type {
            kind: TypeKind::Pointer(PointerType {
                underlying: Box::new(dummy_type_symbol("i32")),
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
        let mut ty = Type {
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
        let mut ty = Type {
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
    fn test_mut_type() {
        let mut ty = Type {
            kind: TypeKind::Mut(MutType {
                underlying: Box::new(dummy_type_symbol("i32")),
            }),
            span: dummy_span(),
        };
        let mut visitor = NodeCounterVisitor::new();
        ty.visit(&mut visitor);
        visitor.assert_visited("type", "Type", 2);
        visitor.assert_visited("type", "MutType", 1);
        visitor.assert_visited("type", "SymbolType", 1);
    }

    #[test]
    fn test_function_type() {
        let mut ty = Type {
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
        let mut ty = Type {
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
        let mut expr = Expr {
            kind: ExprKind::FunctionCall(FunctionCallExpr {
                callee: Box::new(dummy_expr_symbol("foo")),
                arguments: Box::new([
                    dummy_expr_number(1),
                    Expr {
                        kind: ExprKind::StructInstantiation(StructInstantiationExpr {
                            name: dummy_ident("Bar"),
                            properties: HashMap::from([
                                (dummy_ident("x"), dummy_expr_number(2)),
                                (
                                    dummy_ident("y"),
                                    Expr {
                                        kind: ExprKind::Binary(BinaryExpr {
                                            left: Box::new(dummy_expr_number(3)),
                                            operator: dummy_token(
                                                crate::lexer::token::TokenKind::Plus,
                                            ),
                                            right: Box::new(dummy_expr_number(4)),
                                        }),
                                        span: dummy_span(),
                                    },
                                ),
                            ]),
                        }),
                        span: dummy_span(),
                    },
                    Expr {
                        kind: ExprKind::As(AsExpr {
                            expr: Box::new(dummy_expr_symbol("z")),
                            ty: Type {
                                kind: TypeKind::Pointer(PointerType {
                                    underlying: Box::new(dummy_type_symbol("i32")),
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
        let mut ast = Ast(Box::new([
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
                }),
                span: dummy_span(),
                attributes: Box::new([]),
            },
            Stmt {
                kind: StmtKind::StructDecl(StructDeclStmt {
                    name: dummy_ident("Foo"),
                    properties: vec![StructProperty {
                        name: dummy_ident("x"),
                        type_: dummy_type_symbol("i32"),
                        is_public: false,
                    }]
                    .into_boxed_slice(),
                    methods: vec![StructMethod {
                        is_static: false,
                        is_public: false,
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
                            is_public: false,
                            is_extern: false,
                        },
                    }]
                    .into_boxed_slice(),
                    is_public: false,
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
                            body: None,
                            return_type: dummy_type_never(),
                            is_public: false,
                            is_extern: false,
                        },
                    }]
                    .into_boxed_slice(),
                    is_public: false,
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
                                        is_constant: false,
                                        is_public: false,
                                        assigned_value: Some(dummy_expr_number(1)),
                                        type_: dummy_type_infer(),
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
                    is_public: false,
                    is_extern: false,
                }),
                span: dummy_span(),
                attributes: Box::new([]),
            },
        ]));

        let mut visitor = NodeCounterVisitor::new();
        ast.visit(&mut visitor);

        visitor.assert_all_visited(&[
            ("stmt", "ImportStmt", 1),
            ("stmt", "StructDeclStmt", 1),
            ("stmt", "InterfaceDeclStmt", 1),
            ("stmt", "FnDeclStmt", 1),
            ("stmt", "VarDeclStmt", 1),
            ("expr", "BlockExpr", 3),
            ("stmt", "ReturnStmt", 1),
            ("stmt", "ExpressionStmt", 2),
        ]);
    }

    #[test]
    fn test_comprehensive_all_type_types() {
        let mut ty = Type {
            kind: TypeKind::Function(FunctionType {
                parameters: Box::new([
                    Type {
                        kind: TypeKind::Pointer(PointerType {
                            underlying: Box::new(Type {
                                kind: TypeKind::Mut(MutType {
                                    underlying: Box::new(dummy_type_symbol("i32")),
                                }),
                                span: dummy_span(),
                            }),
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
            ("type", "Type", 12),
            ("type", "FunctionType", 1),
            ("type", "PointerType", 1),
            ("type", "MutType", 1),
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
        let mut expr = Expr {
            kind: ExprKind::Binary(BinaryExpr {
                left: Box::new(Expr {
                    kind: ExprKind::Binary(BinaryExpr {
                        left: Box::new(Expr {
                            kind: ExprKind::Binary(BinaryExpr {
                                left: Box::new(dummy_expr_symbol("a")),
                                operator: dummy_token(crate::lexer::token::TokenKind::Plus),
                                right: Box::new(dummy_expr_symbol("b")),
                            }),
                            span: dummy_span(),
                        }),
                        operator: dummy_token(crate::lexer::token::TokenKind::Star),
                        right: Box::new(Expr {
                            kind: ExprKind::Binary(BinaryExpr {
                                left: Box::new(dummy_expr_symbol("c")),
                                operator: dummy_token(crate::lexer::token::TokenKind::Slash),
                                right: Box::new(dummy_expr_symbol("d")),
                            }),
                            span: dummy_span(),
                        }),
                    }),
                    span: dummy_span(),
                }),
                operator: dummy_token(crate::lexer::token::TokenKind::Dash),
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
}
