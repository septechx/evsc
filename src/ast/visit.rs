use std::hash::Hash;

use thin_vec::ThinVec;

use crate::{
    ast::{
        AssocItem, AssocItemKind, Ast, Block, Expr, ExprKind, Fn, Item, ItemKind, Literal, Stmt,
        StmtKind, Type, TypeKind, types::*,
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
    fn visit_item(&mut self, _item: &Item) -> VisitAction {
        VisitAction::Continue
    }
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
        for item in &self.items {
            item.visit(visitor);
        }
    }
}

impl Visitable for Item {
    fn visit(&self, visitor: &mut impl Visitor) {
        match visitor.visit_item(self) {
            VisitAction::Continue => match &self.kind {
                ItemKind::Static { value, ty, .. } => {
                    value.visit(visitor);
                    ty.visit(visitor);
                }
                ItemKind::Struct { fields, items, .. } => {
                    for field in fields {
                        field.1.visit(visitor);
                    }
                    items.visit(visitor);
                }
                ItemKind::Interface { items, .. } => {
                    items.visit(visitor);
                }
                ItemKind::Impl { self_ty, items, .. } => {
                    self_ty.visit(visitor);
                    items.visit(visitor);
                }
                ItemKind::Fn(f) => f.visit(visitor),
                ItemKind::Import(_) => {
                    // Leaf
                }
            },
            VisitAction::SkipChildren => {}
        }
    }
}

impl Visitable for AssocItem {
    fn visit(&self, visitor: &mut impl Visitor) {
        self.kind.visit(visitor);
    }
}

impl Visitable for AssocItemKind {
    fn visit(&self, visitor: &mut impl Visitor) {
        match self {
            AssocItemKind::Fn(f) => f.visit(visitor),
        }
    }
}

impl Visitable for Fn {
    fn visit(&self, visitor: &mut impl Visitor) {
        for arg in &self.parameters {
            arg.1.visit(visitor);
        }
        if let Some(body) = &self.body {
            body.visit(visitor);
        }
        self.return_type.visit(visitor);
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
                StmtKind::Expr(expr) => expr.visit(visitor),
                StmtKind::Semi(expr) => expr.visit(visitor),
                StmtKind::Let {
                    name: _,
                    ty,
                    value,
                    mutability: _,
                } => {
                    if let Some(val) = value {
                        val.visit(visitor);
                    }
                    ty.visit(visitor);
                }
            },
            VisitAction::SkipChildren => {}
        }
    }
}

impl Visitable for Expr {
    fn visit(&self, visitor: &mut impl Visitor) {
        match visitor.visit_expr(self) {
            VisitAction::Continue => match &self.kind {
                ExprKind::Literal(l) => l.visit(visitor),
                ExprKind::Block(b) => b.visit(visitor),
                ExprKind::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    condition.visit(visitor);
                    then_branch.visit(visitor);
                    else_branch.visit(visitor);
                }
                ExprKind::While { condition, body } => {
                    condition.visit(visitor);
                    body.visit(visitor);
                }
                ExprKind::Loop(b) => b.visit(visitor),
                ExprKind::Symbol(_) => {
                    // Leaf
                }
                ExprKind::Binary {
                    left,
                    operator: _,
                    right,
                } => {
                    left.visit(visitor);
                    right.visit(visitor);
                }
                ExprKind::Postfix { left, operator: _ } => {
                    left.visit(visitor);
                }
                ExprKind::Prefix { operator: _, right } => {
                    right.visit(visitor);
                }
                ExprKind::Assignment {
                    assignee, value, ..
                } => {
                    assignee.visit(visitor);
                    value.visit(visitor);
                }
                ExprKind::StructInstantiation { name: _, fields } => {
                    for field in fields {
                        field.1.visit(visitor);
                    }
                }
                ExprKind::ArrayLiteral {
                    underlying,
                    contents,
                } => {
                    underlying.visit(visitor);
                    contents.visit(visitor);
                }
                ExprKind::FunctionCall { callee, parameters } => {
                    callee.visit(visitor);
                    parameters.visit(visitor);
                }
                ExprKind::MemberAccess { base, .. } => {
                    base.visit(visitor);
                }
                ExprKind::Type(t) => t.visit(visitor),
                ExprKind::As { expr, ty } => {
                    expr.visit(visitor);
                    ty.visit(visitor);
                }
                ExprKind::TupleLiteral { elements } => {
                    for element in elements {
                        element.visit(visitor);
                    }
                }
                ExprKind::Break(b) => {
                    if let Some(val) = b {
                        val.visit(visitor);
                    }
                }
                ExprKind::Return(r) => {
                    if let Some(val) = r {
                        val.visit(visitor);
                    }
                }
            },
            VisitAction::SkipChildren => {}
        }
    }
}

impl Visitable for Literal {
    fn visit(&self, _visitor: &mut impl Visitor) {
        // Unit
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
