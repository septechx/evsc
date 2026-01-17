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
    /// Delegates visitation to the boxed inner value by calling `visit` on the contained value.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given a Box<T> where T: Visitable, calling `visit` on the box forwards the call:
    /// // let mut boxed = Box::new(some_visitable_value);
    /// // let mut visitor = /* implementor of Visitor */;
    /// // boxed.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.as_mut().visit(visitor);
    }
}

impl<T: Visitable> Visitable for Option<T> {
    /// Visits the contained value if the `Option` is `Some`.
    ///
    /// # Examples
    ///
    /// ```
    /// // Minimal local Visitor/Visitable setup for demonstration.
    /// trait Visitor { fn seen(&mut self, _n: &i32); }
    /// trait Visitable { fn visit(&mut self, visitor: &mut dyn Visitor); }
    ///
    /// impl Visitable for i32 {
    ///     fn visit(&mut self, visitor: &mut dyn Visitor) { visitor.seen(self); }
    /// }
    ///
    /// impl<T: Visitable> Visitable for Option<T> {
    ///     fn visit(&mut self, visitor: &mut dyn Visitor) {
    ///         if let Some(inner) = self { inner.visit(visitor); }
    ///     }
    /// }
    ///
    /// struct Cnt { pub c: usize }
    /// impl Visitor for Cnt { fn seen(&mut self, _n: &i32) { self.c += 1; } }
    ///
    /// let mut v = Cnt { c: 0 };
    /// let mut some = Some(42i32);
    /// some.visit(&mut v);
    /// assert_eq!(v.c, 1);
    ///
    /// let mut none: Option<i32> = None;
    /// none.visit(&mut v);
    /// assert_eq!(v.c, 1);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        if let Some(inner) = self {
            inner.visit(visitor);
        }
    }
}

impl<T: Visitable> Visitable for Vec<T> {
    /// Visits each element of the collection by delegating to the provided visitor.
    ///
    /// # Examples
    ///
    /// ```
    /// struct Dummy(i32);
    ///
    /// struct Noop;
    /// impl Visitor for Noop {
    ///     fn visit_stmt(&mut self, _s: &mut Stmt) {}
    ///     fn visit_expr(&mut self, _e: &mut Expr) {}
    ///     fn visit_type(&mut self, _t: &mut Type) {}
    /// }
    ///
    /// impl Visitable for Dummy {
    ///     fn visit(&mut self, _v: &mut impl Visitor) {
    ///         // element-specific visitation (no-op for this example)
    ///     }
    /// }
    ///
    /// let mut items = vec![Dummy(1), Dummy(2)];
    /// let mut visitor = Noop;
    /// items.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for inner in self {
            inner.visit(visitor);
        }
    }
}

impl<K: Eq + Hash, V: Visitable> Visitable for HashMap<K, V> {
    /// Visits each value stored in the map with the given visitor.
    ///
    /// Iterates over all mutable values and calls their `visit` method with `visitor`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::collections::HashMap;
    ///
    /// trait Visitor {}
    /// trait Visitable { fn visit(&mut self, _v: &mut impl Visitor) {} }
    ///
    /// impl<K, V: Visitable> Visitable for HashMap<K, V> {
    ///     fn visit(&mut self, v: &mut impl Visitor) {
    ///         for (_, value) in self.iter_mut() {
    ///             value.visit(v);
    ///         }
    ///     }
    /// }
    ///
    /// struct MyValue { visited: bool }
    /// impl Visitable for MyValue {
    ///     fn visit(&mut self, _v: &mut impl Visitor) { self.visited = true; }
    /// }
    ///
    /// struct MyVisitor;
    /// impl Visitor for MyVisitor {}
    ///
    /// let mut map: HashMap<i32, MyValue> = HashMap::new();
    /// map.insert(1, MyValue { visited: false });
    /// map.visit(&mut MyVisitor);
    /// assert!(map.get_mut(&1).unwrap().visited);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for (_, value) in self.iter_mut() {
            value.visit(visitor);
        }
    }
}

impl Visitable for Ast {
    /// Visits each top-level statement in the AST with the provided visitor.
    ///
    /// This traverses the AST's statement list and invokes `visit` on every statement,
    /// allowing the visitor to process or mutate each node.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given an `Ast` and a `visitor` that implements `Visitor`:
    /// // let mut ast = Ast(vec![/* statements */]);
    /// // let mut visitor = MyVisitor::new();
    /// // ast.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for stmt in &mut self.0 {
            stmt.visit(visitor);
        }
    }
}

impl Visitable for Stmt {
    /// Accepts a visitor to traverse this statement and its nested nodes.
    ///
    /// Calls `visitor.visit_stmt(self)` and then recursively visits child nodes according to the statement kind (e.g., block body, expression, variable declaration, struct/interface/function declarations, return).
    ///
    /// # Examples
    ///
    /// ```
    /// struct Dummy;
    /// impl Visitor for Dummy {
    ///     fn visit_stmt(&mut self, _s: &mut Stmt) {}
    ///     fn visit_expr(&mut self, _e: &mut Expr) {}
    ///     fn visit_type(&mut self, _t: &mut Type) {}
    /// }
    ///
    /// let mut visitor = Dummy;
    /// let mut stmt = /* construct or obtain a `Stmt` */ ;
    /// stmt.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_stmt(self);

        match &mut self.kind {
            StmtKind::Block(block) => block.visit(visitor),
            StmtKind::Expression(expr_stmt) => expr_stmt.visit(visitor),
            StmtKind::VarDecl(var) => var.visit(visitor),
            StmtKind::StructDecl(s) => s.visit(visitor),
            StmtKind::InterfaceDecl(i) => i.visit(visitor),
            StmtKind::FnDecl(f) => f.visit(visitor),
            StmtKind::Return(r) => r.visit(visitor),
            StmtKind::Import(_) => {}
        }
    }
}

impl Visitable for BlockStmt {
    /// Visits each statement in the block's body with the given visitor.
    ///
    /// # Examples
    ///
    /// ```
    /// struct CountingVisitor { count: usize }
    ///
    /// impl Visitor for CountingVisitor {
    ///     fn visit_stmt(&mut self, _s: &mut Stmt) { self.count += 1; }
    ///     fn visit_expr(&mut self, _e: &mut Expr) {}
    ///     fn visit_type(&mut self, _t: &mut Type) {}
    /// }
    ///
    /// let mut block = BlockStmt { body: Vec::new() };
    /// let mut visitor = CountingVisitor { count: 0 };
    /// block.visit(&mut visitor);
    /// assert_eq!(visitor.count, 0);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.body.visit(visitor);
    }
}

impl Visitable for ExpressionStmt {
    /// Visits the contained expression using the provided visitor.
    ///
    /// # Examples
    ///
    /// ```
    /// // Minimal example demonstrating the call pattern.
    /// trait Visitor { fn visit_expr(&mut self, _e: &mut Expr) {} }
    /// struct DummyVisitor;
    /// impl Visitor for DummyVisitor {}
    ///
    /// struct Expr;
    /// impl Expr {
    ///     fn visit(&mut self, _visitor: &mut impl Visitor) {
    ///         // no-op for example
    ///     }
    /// }
    ///
    /// struct ExpressionStmt { expression: Expr }
    /// impl ExpressionStmt {
    ///     fn visit(&mut self, visitor: &mut impl Visitor) {
    ///         self.expression.visit(visitor);
    ///     }
    /// }
    ///
    /// let mut stmt = ExpressionStmt { expression: Expr };
    /// let mut visitor = DummyVisitor;
    /// stmt.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.expression.visit(visitor);
    }
}

impl Visitable for VarDeclStmt {
    /// Visits the optional assigned value (if any) and then visits the declared type.
    ///
    /// This method first visits `assigned_value` when present, then always visits `type_`,
    /// allowing a `Visitor` to traverse the initializer expression followed by the variable's type.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// // Given a VarDeclStmt `stmt` and a `visitor` implementing `Visitor`:
    /// // stmt.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        if let Some(val) = &mut self.assigned_value {
            val.visit(visitor);
        }
        self.type_.visit(visitor);
    }
}

impl Visitable for StructProperty {
    /// Forwards AST visitation to the underlying type.
    ///
    /// This method delegates traversal to the inner `type_` field so the visitor visits
    /// the nested type node.
    ///
    /// # Examples
    ///
    /// ```
    /// // Suppose `mut_type` wraps an inner type node and `v` implements `Visitor`,
    /// // calling `mut_type.visit(&mut v)` will cause `v` to visit the inner type.
    /// // (Concrete types omitted for brevity.)
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.type_.visit(visitor);
    }
}

impl Visitable for StructMethod {
    /// Visits the method's function declaration with the provided visitor.
    ///
    /// This delegates traversal to the contained function declaration so the visitor
    /// can traverse the method body, parameters, and return type.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given `method` is a `StructMethod` and `visitor` implements `Visitor`:
    /// // method.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.fn_decl.visit(visitor);
    }
}

impl Visitable for StructDeclStmt {
    /// Visits each property and method of the struct using the provided visitor.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given a mutable `struct_decl: StructDeclStmt` and a mutable visitor `v`:
    /// // struct_decl.visit(&mut v);
    /// ```
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
    /// Visits the method's function declaration with the provided visitor.
    ///
    /// This delegates traversal to the contained function declaration so the visitor
    /// can traverse the method body, parameters, and return type.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given `method` is a `StructMethod` and `visitor` implements `Visitor`:
    /// // method.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.fn_decl.visit(visitor);
    }
}

impl Visitable for InterfaceDeclStmt {
    /// Visits each method in the struct declaration using the provided visitor.
    ///
    /// Invokes `visit` on every method in `self.methods` in order.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given a struct declaration `stmt` and a mutable visitor `v`,
    /// // calling `stmt.visit(&mut v)` will forward the visitor to each method:
    /// // stmt.visit(&mut v);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for m in &mut self.methods {
            m.visit(visitor);
        }
    }
}

impl Visitable for FnArgument {
    /// Forwards AST visitation to the underlying type.
    ///
    /// This method delegates traversal to the inner `type_` field so the visitor visits
    /// the nested type node.
    ///
    /// # Examples
    ///
    /// ```
    /// // Suppose `mut_type` wraps an inner type node and `v` implements `Visitor`,
    /// // calling `mut_type.visit(&mut v)` will cause `v` to visit the inner type.
    /// // (Concrete types omitted for brevity.)
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.type_.visit(visitor);
    }
}

impl Visitable for FnDeclStmt {
    /// Visits the function declaration's contents with the provided visitor.
    ///
    /// This visits each function argument, then each statement in the function body, and finally the function's return type.
    ///
    /// # Examples
    ///
    /// ```
    /// // Create a visitor and a function declaration, then walk the function's nodes.
    /// // (Constructors omitted for brevity; use your AST builders.)
    /// let mut visitor = MyVisitor::new();
    /// let mut fn_decl = FnDeclStmt { /* fields */ };
    /// fn_decl.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for arg in &mut self.arguments {
            arg.visit(visitor);
        }
        for stmt in &mut self.body {
            stmt.visit(visitor);
        }
        self.return_type.visit(visitor);
    }
}

impl Visitable for ReturnStmt {
    /// Forwards visitation to the contained value when present.
    ///
    /// If `value` is `Some`, calls the inner value's `visit` method with the given visitor; does nothing if `value` is `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// // Assuming a type `Wrapper` with a field `value: Option<T>` where `T: Visitable`,
    /// // calling `wrapper.visit(&mut visitor)` will visit the inner `T` when present.
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        if let Some(v) = &mut self.value {
            v.visit(visitor);
        }
    }
}

impl Visitable for Expr {
    /// Accepts a visitor and recursively visits this expression and any nested expressions or types.
    ///
    /// Calls the visitor for this expression and then traverses child expressions and types where applicable.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// struct PrintVisitor;
    /// impl Visitor for PrintVisitor {
    ///     fn visit_expr(&mut self, _e: &mut Expr) {}
    ///     fn visit_type(&mut self, _t: &mut Type) {}
    ///     fn visit_stmt(&mut self, _s: &mut Stmt) {}
    /// }
    ///
    /// let mut expr = Expr { kind: ExprKind::Number(42) };
    /// let mut visitor = PrintVisitor;
    /// expr.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        visitor.visit_expr(self);

        match &mut self.kind {
            ExprKind::Number(_) => {}
            ExprKind::String(_) => {}
            ExprKind::Symbol(_) => {}
            ExprKind::Binary(bin) => {
                bin.left.visit(visitor);
                bin.right.visit(visitor);
            }
            ExprKind::Postfix(p) => {
                p.left.visit(visitor);
            }
            ExprKind::Prefix(p) => {
                p.right.visit(visitor);
            }
            ExprKind::Assignment(a) => {
                a.assigne.visit(visitor);
                a.value.visit(visitor);
            }
            ExprKind::StructInstantiation(si) => {
                si.properties.visit(visitor);
            }
            ExprKind::ArrayLiteral(arr) => {
                arr.underlying.visit(visitor);
                arr.contents.visit(visitor);
            }
            ExprKind::FunctionCall(fc) => {
                fc.callee.visit(visitor);
                fc.arguments.visit(visitor);
            }
            ExprKind::MemberAccess(ma) => {
                ma.base.visit(visitor);
            }
            ExprKind::Type(t) => {
                t.underlying.visit(visitor);
            }
            ExprKind::As(asexpr) => {
                asexpr.expr.visit(visitor);
                asexpr.ty.visit(visitor);
            }
        }
    }
}

impl Visitable for NumberExpr {
    /// Accepts a visitor but does not traverse any child nodes.
///
/// This implementation intentionally performs no actions because the visited node has no children or nested
/// elements to visit.
///
/// # Examples
///
/// ```
/// // Calling `visit` on this node is a no-op:
/// // node.visit(&mut visitor);
/// ```
fn visit(&mut self, _visitor: &mut impl Visitor) {}
}
impl Visitable for StringExpr {
    /// Accepts a visitor but does not traverse any child nodes.
///
/// This implementation intentionally performs no actions because the visited node has no children or nested
/// elements to visit.
///
/// # Examples
///
/// ```
/// // Calling `visit` on this node is a no-op:
/// // node.visit(&mut visitor);
/// ```
fn visit(&mut self, _visitor: &mut impl Visitor) {}
}
impl Visitable for SymbolExpr {
    /// Accepts a visitor but does not traverse any child nodes.
///
/// This implementation intentionally performs no actions because the visited node has no children or nested
/// elements to visit.
///
/// # Examples
///
/// ```
/// // Calling `visit` on this node is a no-op:
/// // node.visit(&mut visitor);
/// ```
fn visit(&mut self, _visitor: &mut impl Visitor) {}
}

impl Visitable for BinaryExpr {
    /// Visits the left and right subexpressions of this binary expression.
    ///
    /// # Examples
    ///
    /// ```
    /// struct DummyVisitor;
    ///
    /// impl Visitor for DummyVisitor {
    ///     fn visit_stmt(&mut self, _s: &mut Stmt) {}
    ///     fn visit_expr(&mut self, _e: &mut Expr) {}
    ///     fn visit_type(&mut self, _t: &mut Type) {}
    /// }
    ///
    /// let mut bin = BinaryExpr { left: Box::new(NumberExpr::new(1)), right: Box::new(NumberExpr::new(2)), ..Default::default() };
    /// let mut visitor = DummyVisitor;
    /// bin.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.left.visit(visitor);
        self.right.visit(visitor);
    }
}
impl Visitable for PostfixExpr {
    /// Visits the node's left subexpression with the given visitor.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// // Construct a node that implements this `visit` (e.g., a postfix expression),
    /// // create a Visitor implementation, then call `visit`.
    /// let mut node = /* construct node with a `left` subexpression */ ;
    /// let mut visitor = /* implement Visitor */ ;
    /// node.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.left.visit(visitor);
    }
}
impl Visitable for PrefixExpr {
    /// Visits the node's right-hand subexpression with the provided visitor.
    ///
    /// The visitor will be applied to the right child of this node; no other children are visited.
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.right.visit(visitor);
    }
}
impl Visitable for AssignmentExpr {
    /// Visits the assignee and the value of an assignment expression.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given an assignment expression `assign` with `assigne` and `value`
    /// // fields that implement `Visitable`, calling `visit` will visit both:
    /// let mut visitor = crate::visitor::NoopVisitor::default();
    /// let mut assign = crate::ast::AssignmentExpr {
    ///     assigne: Box::new(crate::ast::SymbolExpr::new("x")),
    ///     value: Box::new(crate::ast::NumberExpr::new(1.0)),
    /// };
    /// assign.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.assigne.visit(visitor);
        self.value.visit(visitor);
    }
}
impl Visitable for StructInstantiationExpr {
    /// Visits all properties of this struct declaration using the given visitor.
    ///
    /// Delegates traversal to the struct's `properties`, invoking each property's `visit`
    /// implementation with the provided `visitor`.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given a type that implements `Visitor`, call `visit` on a `StructDecl` to
    /// // traverse its properties:
    /// // let mut visitor = MyVisitor::new();
    /// // let mut struct_decl = StructDecl { properties: vec![ /* ... */ ], /* ... */ };
    /// // struct_decl.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.properties.visit(visitor);
    }
}
impl Visitable for ArrayLiteralExpr {
    /// Visits the fixed-size array's underlying type then its element type.
    ///
    /// This invokes the provided `Visitor` on the array's `underlying` type followed by its `contents`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// // Given a `fixed_array` that implements `Visitable`:
    /// let mut visitor = MyVisitor::new();
    /// fixed_array.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
        self.contents.visit(visitor);
    }
}
impl Visitable for FunctionCallExpr {
    /// Visits the callee and the arguments of this function call expression with the given visitor.
    ///
    /// # Examples
    ///
    /// ```
    /// // Minimal types to demonstrate `visit` usage.
    /// trait Visitor {}
    /// trait Visitable { fn visit(&mut self, visitor: &mut impl Visitor); }
    ///
    /// struct DummyCallee;
    /// impl Visitable for DummyCallee {
    ///     fn visit(&mut self, _visitor: &mut impl Visitor) {}
    /// }
    ///
    /// struct DummyArg;
    /// impl Visitable for DummyArg {
    ///     fn visit(&mut self, _visitor: &mut impl Visitor) {}
    /// }
    ///
    /// struct FunctionCall {
    ///     callee: DummyCallee,
    ///     arguments: Vec<DummyArg>,
    /// }
    ///
    /// impl FunctionCall {
    ///     fn visit(&mut self, visitor: &mut impl Visitor) {
    ///         self.callee.visit(visitor);
    ///         self.arguments.visit(visitor);
    ///     }
    /// }
    ///
    /// // Use a concrete visitor implementation (empty for this example).
    /// struct MyVisitor;
    /// impl Visitor for MyVisitor {}
    ///
    /// let mut call = FunctionCall { callee: DummyCallee, arguments: vec![DummyArg] };
    /// let mut visitor = MyVisitor;
    /// call.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.callee.visit(visitor);
        self.arguments.visit(visitor);
    }
}
impl Visitable for MemberAccessExpr {
    /// Delegates AST visitation to the wrapped `base` node.
    ///
    /// This method forwards the provided `visitor` to `self.base`, allowing the visitor to continue traversal from the underlying node.
    ///
    /// # Examples
    ///
    /// ```
    /// // Minimal local definitions to demonstrate forwarding behavior.
    /// trait Visitor {}
    /// trait Visitable { fn visit(&mut self, visitor: &mut dyn Visitor); }
    ///
    /// struct Base;
    /// impl Visitable for Base {
    ///     fn visit(&mut self, _visitor: &mut dyn Visitor) {}
    /// }
    ///
    /// struct Wrapper { base: Base }
    /// impl Wrapper {
    ///     fn visit(&mut self, visitor: &mut dyn Visitor) {
    ///         self.base.visit(visitor);
    ///     }
    /// }
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.base.visit(visitor);
    }
}
impl Visitable for TypeExpr {
    /// Delegates visitation to the underlying value.
    ///
    /// # Examples
    ///
    /// ```
    /// trait Visitor {}
    /// trait Visitable { fn visit(&mut self, _v: &mut dyn Visitor) {} }
    ///
    /// struct Wrapper<T: Visitable> { underlying: T }
    ///
    /// impl<T: Visitable> Wrapper<T> {
    ///     fn visit(&mut self, visitor: &mut dyn Visitor) {
    ///         self.underlying.visit(visitor);
    ///     }
    /// }
    ///
    /// // Usage (illustrative)
    /// // let mut wrapper = Wrapper { underlying: /* some Visitable */ };
    /// // let mut visitor = /* some Visitor */;
    /// // wrapper.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for AsExpr {
    /// Visits the inner expression followed by the target type using the provided visitor.
    ///
    /// # Examples
    ///
    /// ```
    /// // Given `as_expr` is an AST "as" expression node and `visitor` implements `Visitor`:
    /// as_expr.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.expr.visit(visitor);
        self.ty.visit(visitor);
    }
}

impl Visitable for Type {
    /// Invokes the visitor for this type and recursively visits any nested/underlying types.
    ///
    /// Calls `visitor.visit_type(self)` and then traverses the type's children according to its
    /// `TypeKind` (e.g., underlying type of pointers/slices/arrays/mut, function parameters and
    /// return type).
    ///
    /// # Examples
    ///
    /// ```no_run
    /// // Given a `Type` value `ty` and a mutable visitor `v`:
    /// // ty.visit(&mut v);
    /// ```
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
            TypeKind::Infer => {}
            TypeKind::Never => {}
        }
    }
}

impl Visitable for SymbolType {
    /// Accepts a visitor but does not traverse any child nodes.
///
/// This implementation intentionally performs no actions because the visited node has no children or nested
/// elements to visit.
///
/// # Examples
///
/// ```
/// // Calling `visit` on this node is a no-op:
/// // node.visit(&mut visitor);
/// ```
fn visit(&mut self, _visitor: &mut impl Visitor) {}
}
impl Visitable for PointerType {
    /// Delegates visitation to the underlying value.
    ///
    /// # Examples
    ///
    /// ```
    /// trait Visitor {}
    /// trait Visitable { fn visit(&mut self, _v: &mut dyn Visitor) {} }
    ///
    /// struct Wrapper<T: Visitable> { underlying: T }
    ///
    /// impl<T: Visitable> Wrapper<T> {
    ///     fn visit(&mut self, visitor: &mut dyn Visitor) {
    ///         self.underlying.visit(visitor);
    ///     }
    /// }
    ///
    /// // Usage (illustrative)
    /// // let mut wrapper = Wrapper { underlying: /* some Visitable */ };
    /// // let mut visitor = /* some Visitor */;
    /// // wrapper.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for SliceType {
    /// Delegates visitation to the underlying value.
    ///
    /// # Examples
    ///
    /// ```
    /// trait Visitor {}
    /// trait Visitable { fn visit(&mut self, _v: &mut dyn Visitor) {} }
    ///
    /// struct Wrapper<T: Visitable> { underlying: T }
    ///
    /// impl<T: Visitable> Wrapper<T> {
    ///     fn visit(&mut self, visitor: &mut dyn Visitor) {
    ///         self.underlying.visit(visitor);
    ///     }
    /// }
    ///
    /// // Usage (illustrative)
    /// // let mut wrapper = Wrapper { underlying: /* some Visitable */ };
    /// // let mut visitor = /* some Visitor */;
    /// // wrapper.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for FixedArrayType {
    /// Delegates visitation to the underlying value.
    ///
    /// # Examples
    ///
    /// ```
    /// trait Visitor {}
    /// trait Visitable { fn visit(&mut self, _v: &mut dyn Visitor) {} }
    ///
    /// struct Wrapper<T: Visitable> { underlying: T }
    ///
    /// impl<T: Visitable> Wrapper<T> {
    ///     fn visit(&mut self, visitor: &mut dyn Visitor) {
    ///         self.underlying.visit(visitor);
    ///     }
    /// }
    ///
    /// // Usage (illustrative)
    /// // let mut wrapper = Wrapper { underlying: /* some Visitable */ };
    /// // let mut visitor = /* some Visitor */;
    /// // wrapper.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for MutType {
    /// Delegates visitation to the underlying value.
    ///
    /// # Examples
    ///
    /// ```
    /// trait Visitor {}
    /// trait Visitable { fn visit(&mut self, _v: &mut dyn Visitor) {} }
    ///
    /// struct Wrapper<T: Visitable> { underlying: T }
    ///
    /// impl<T: Visitable> Wrapper<T> {
    ///     fn visit(&mut self, visitor: &mut dyn Visitor) {
    ///         self.underlying.visit(visitor);
    ///     }
    /// }
    ///
    /// // Usage (illustrative)
    /// // let mut wrapper = Wrapper { underlying: /* some Visitable */ };
    /// // let mut visitor = /* some Visitor */;
    /// // wrapper.visit(&mut visitor);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        self.underlying.visit(visitor);
    }
}
impl Visitable for FunctionType {
    /// Visits each parameter type and then the return type using the provided visitor.
    ///
    /// This method invokes `visit` on every entry in `self.parameters` in order, then invokes
    /// `visit` on `self.return_type`.
    ///
    /// # Examples
    ///
    /// ```
    /// // Minimal local definitions to demonstrate usage in documentation.
    /// trait Visitor { fn visit_type(&mut self, _t: &mut Type) {} fn visit_stmt(&mut self, _s: &mut Stmt) {} fn visit_expr(&mut self, _e: &mut Expr) {} }
    /// trait Visitable { fn visit(&mut self, visitor: &mut impl Visitor); }
    /// struct Type { pub id: u8 }
    /// impl Visitable for Type { fn visit(&mut self, visitor: &mut impl Visitor) { visitor.visit_type(self) } }
    /// struct FunctionType { parameters: Vec<Type>, return_type: Type }
    /// impl FunctionType {
    ///     fn visit(&mut self, visitor: &mut impl Visitor) {
    ///         for p in &mut self.parameters { p.visit(visitor); }
    ///         self.return_type.visit(visitor);
    ///     }
    /// }
    ///
    /// // Use a visitor that counts type visits.
    /// struct CountingVisitor { pub count: usize }
    /// impl Visitor for CountingVisitor {
    ///     fn visit_type(&mut self, _t: &mut Type) { self.count += 1; }
    /// }
    ///
    /// let mut f = FunctionType { parameters: vec![Type { id: 1 }, Type { id: 2 }], return_type: Type { id: 3 } };
    /// let mut v = CountingVisitor { count: 0 };
    /// f.visit(&mut v);
    /// assert_eq!(v.count, 3);
    /// ```
    fn visit(&mut self, visitor: &mut impl Visitor) {
        for p in &mut self.parameters {
            p.visit(visitor);
        }
        self.return_type.visit(visitor);
    }
}