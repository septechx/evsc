use std::hash::Hash;

use thin_vec::ThinVec;

use crate::{
    ast::{
        Ast, Block, Expr, ExprKind, Literal, Stmt, StmtKind, Type, TypeKind, expressions::*,
        statements::*, types::*,
    },
    hashmap::FxHashMap,
};

pub enum VisitAction {
    /// Descend into children
    Continue,
    /// Don't descend
    SkipChildren,
}

pub trait Visitor {
    fn visit_stmt(&mut self, _stmt: &Stmt) -> VisitAction {
        VisitAction::Continue
    }
    fn visit_expr(&mut self, _expr: &Expr) -> VisitAction {
        VisitAction::Continue
    }
    fn visit_type(&mut self, _ty: &Type) -> VisitAction {
        VisitAction::Continue
    }
}

pub trait Visitable {
    fn visit(&self, visitor: &mut impl Visitor);
}

impl<T: Visitable> Visitable for Box<T> {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.as_ref().visit(visitor);
    }
}

impl<T: Visitable> Visitable for Option<T> {
    fn visit(&self, visitor: &mut impl Visitor) {
        if let Some(inner) = self {
            inner.visit(visitor);
        }
    }
}

impl<T: Visitable> Visitable for ThinVec<T> {
    fn visit(&self, visitor: &mut impl Visitor) {
        for inner in self {
            inner.visit(visitor);
        }
    }
}

impl<T: Visitable> Visitable for Vec<T> {
    fn visit(&self, visitor: &mut impl Visitor) {
        for inner in self {
            inner.visit(visitor);
        }
    }
}

impl<T: Visitable> Visitable for Box<[T]> {
    fn visit(&self, visitor: &mut impl Visitor) {
        for inner in self.iter() {
            inner.visit(visitor);
        }
    }
}

impl<K: Eq + Hash, V: Visitable> Visitable for FxHashMap<K, V> {
    fn visit(&self, visitor: &mut impl Visitor) {
        for (_, value) in self.iter() {
            value.visit(visitor);
        }
    }
}

impl Visitable for Ast {
    fn visit(&self, visitor: &mut impl Visitor) {
        for stmt in &self.items {
            stmt.visit(visitor);
        }
    }
}

impl Visitable for Block {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.stmts.visit(visitor);
    }
}

impl Visitable for Stmt {
    fn visit(&self, visitor: &mut impl Visitor) {
        match visitor.visit_stmt(self) {
            VisitAction::Continue => match &self.kind {
                StmtKind::Expr(expr_stmt) => expr_stmt.visit(visitor),
                StmtKind::Semi(semi_stmt) => semi_stmt.visit(visitor),
                StmtKind::VarDecl(var) => var.visit(visitor),
                StmtKind::StructDecl(s) => s.visit(visitor),
                StmtKind::InterfaceDecl(i) => i.visit(visitor),
                StmtKind::Impl(i) => i.visit(visitor),
                StmtKind::FnDecl(f) => f.visit(visitor),
                StmtKind::Import(i) => i.visit(visitor),
            },
            VisitAction::SkipChildren => {}
        }
    }
}

impl Visitable for ImportStmt {
    fn visit(&self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}

impl Visitable for BlockExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.block.visit(visitor);
    }
}

impl Visitable for ExprStmt {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.expr.visit(visitor);
    }
}

impl Visitable for SemiStmt {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.expr.visit(visitor);
    }
}

impl Visitable for VarDeclStmt {
    fn visit(&self, visitor: &mut impl Visitor) {
        if let Some(val) = &self.assigned_value {
            val.visit(visitor);
        }
        self.ty.visit(visitor);
    }
}

impl Visitable for StructField {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.ty.visit(visitor);
    }
}

impl Visitable for StructMethod {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.fn_decl.visit(visitor);
    }
}

impl Visitable for StructDeclStmt {
    fn visit(&self, visitor: &mut impl Visitor) {
        for p in &self.fields {
            p.visit(visitor);
        }
        for m in &self.methods {
            m.visit(visitor);
        }
    }
}

impl Visitable for InterfaceMethod {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.fn_decl.visit(visitor);
    }
}

impl Visitable for InterfaceDeclStmt {
    fn visit(&self, visitor: &mut impl Visitor) {
        for m in &self.methods {
            m.visit(visitor);
        }
    }
}

impl Visitable for ImplStmt {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.self_ty.visit(visitor);
        for item in &self.items {
            item.visit(visitor);
        }
    }
}

impl Visitable for FnParameter {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.ty.visit(visitor);
    }
}

impl Visitable for FnDeclStmt {
    fn visit(&self, visitor: &mut impl Visitor) {
        for arg in &self.parameters {
            arg.visit(visitor);
        }
        if let Some(body) = &self.body {
            body.visit(visitor);
        }
        self.return_type.visit(visitor);
    }
}

impl Visitable for Expr {
    fn visit(&self, visitor: &mut impl Visitor) {
        match visitor.visit_expr(self) {
            VisitAction::Continue => match &self.kind {
                ExprKind::Literal(l) => l.visit(visitor),
                ExprKind::Block(b) => b.visit(visitor),
                ExprKind::If(i) => i.visit(visitor),
                ExprKind::While(w) => w.visit(visitor),
                ExprKind::Loop(l) => l.visit(visitor),
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
                ExprKind::Break(b) => b.visit(visitor),
                ExprKind::Return(r) => r.visit(visitor),
            },
            VisitAction::SkipChildren => {}
        }
    }
}

impl Visitable for SymbolExpr {
    fn visit(&self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}

impl Visitable for Literal {
    fn visit(&self, _visitor: &mut impl Visitor) {
        // Unit
    }
}

impl Visitable for IfExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.condition.visit(visitor);
        self.then_branch.visit(visitor);
        self.else_branch.visit(visitor);
    }
}

impl Visitable for WhileExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.condition.visit(visitor);
        self.body.visit(visitor);
    }
}

impl Visitable for LoopExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.body.visit(visitor);
    }
}

impl Visitable for BreakExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        if let Some(v) = &self.value {
            v.visit(visitor);
        }
    }
}

impl Visitable for ReturnExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        if let Some(v) = &self.value {
            v.visit(visitor);
        }
    }
}

impl Visitable for BinaryExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.left.visit(visitor);
        self.right.visit(visitor);
    }
}
impl Visitable for PostfixExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.left.visit(visitor);
    }
}
impl Visitable for PrefixExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.right.visit(visitor);
    }
}
impl Visitable for AssignmentExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.assigne.visit(visitor);
        self.value.visit(visitor);
    }
}
impl Visitable for StructInstantiationExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.fields.visit(visitor);
    }
}
impl Visitable for ArrayLiteralExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
        self.contents.visit(visitor);
    }
}
impl Visitable for FunctionCallExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.callee.visit(visitor);
        self.parameters.visit(visitor);
    }
}
impl Visitable for MemberAccessExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.base.visit(visitor);
    }
}
impl Visitable for TypeExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for AsExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.expr.visit(visitor);
        self.ty.visit(visitor);
    }
}

impl Visitable for TupleLiteralExpr {
    fn visit(&self, visitor: &mut impl Visitor) {
        for element in &self.elements {
            element.visit(visitor);
        }
    }
}

impl Visitable for Type {
    fn visit(&self, visitor: &mut impl Visitor) {
        match visitor.visit_type(self) {
            VisitAction::Continue => match &self.kind {
                TypeKind::Symbol(_) => {}
                TypeKind::Pointer(p) => p.underlying.visit(visitor),
                TypeKind::Slice(s) => s.underlying.visit(visitor),
                TypeKind::FixedArray(f) => {
                    f.underlying.visit(visitor);
                }
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
            },
            VisitAction::SkipChildren => {}
        }
    }
}

impl Visitable for SymbolType {
    fn visit(&self, _visitor: &mut impl Visitor) {
        // Leaf
    }
}

impl Visitable for PointerType {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}

impl Visitable for SliceType {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}

impl Visitable for FixedArrayType {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}

impl Visitable for FunctionType {
    fn visit(&self, visitor: &mut impl Visitor) {
        for p in &self.parameters {
            p.visit(visitor);
        }
        self.return_type.visit(visitor);
    }
}

impl Visitable for TupleType {
    fn visit(&self, visitor: &mut impl Visitor) {
        for element in &self.elements {
            element.visit(visitor);
        }
    }
}
