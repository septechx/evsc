use std::hash::Hash;

use crate::{
    ast::{
        Ast, Expr, ExprKind, Literal, Stmt, StmtKind, Type, TypeKind, expressions::*,
        statements::*, types::*,
    },
    hashmap::FxHashMap,
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

impl<K: Eq + Hash, V: Visitable> Visitable for FxHashMap<K, V> {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for (_, value) in self.iter_mut() {
            value.visit(visitor);
        }
    }
}

impl Visitable for Ast {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for stmt in &mut self.items {
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
            ExprKind::Literal(l) => l.visit(visitor),
            ExprKind::Block(block) => block.visit(visitor),
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

impl Visitable for SymbolExpr {
    fn visit(&mut self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}

impl Visitable for Literal {
    fn visit(&mut self, _visitor: &mut impl Visitor) {
        // Unit
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
