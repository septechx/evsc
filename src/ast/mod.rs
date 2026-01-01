pub mod expressions;
pub mod statements;
pub mod types;
pub mod visit;

use crate::{
    ast::{expressions::*, statements::*, types::*},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct Ast(pub Vec<Stmt>);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Copy)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute {
    pub name: Box<str>,
    pub arguments: Option<Vec<Box<str>>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub id: NodeId,
    pub span: Span,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Block(BlockStmt),
    Expression(ExpressionStmt),
    VarDecl(VarDeclStmt),
    StructDecl(StructDeclStmt),
    InterfaceDecl(InterfaceDeclStmt),
    FnDecl(FnDeclStmt),
    Return(ReturnStmt),
    Import(ImportStmt),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(NumberExpr),
    String(StringExpr),
    Symbol(SymbolExpr),
    Binary(BinaryExpr),
    Postfix(PostfixExpr),
    Prefix(PrefixExpr),
    Assignment(AssignmentExpr),
    StructInstantiation(StructInstantiationExpr),
    ArrayLiteral(ArrayLiteralExpr),
    FunctionCall(FunctionCallExpr),
    MemberAccess(MemberAccessExpr),
    Type(TypeExpr),
    As(AsExpr),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Symbol(SymbolType),
    Pointer(PointerType),
    Slice(SliceType),
    FixedArray(FixedArrayType),
    Mut(MutType),
    Function(FunctionType),
    Infer,
    Never,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub value: SymbolExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<Ident>,
}

#[derive(Debug, Clone)]
pub enum ImportTreeKind {
    /// `import prefix` or `import prefix as rename`
    ///
    /// The inner value represents the rename if it exists.
    Simple(Option<Ident>),
    /// `import prefix::{...}`
    ///
    /// The span represents the braces of the nested group and all elements within:
    ///
    /// ```text
    /// import foo::{bar, baz};
    ///             ^^^^^^^^^^
    /// ```
    Nested {
        items: Vec<(ImportTree, NodeId)>,
        span: Span,
    },
    /// `import prefix::*`
    Glob,
}

#[derive(Debug, Clone)]
pub struct ImportTree {
    pub prefix: Path,
    pub kind: ImportTreeKind,
    pub span: Span,
}

impl ImportTree {
    pub fn ident(&self) -> Ident {
        match &self.kind {
            ImportTreeKind::Simple(Some(rename)) => rename.clone(),
            ImportTreeKind::Simple(None) => self
                .prefix
                .segments
                .last()
                .expect("empty prefix in a simple import")
                .clone(),
            _ => panic!("`UseTree::ident` can only be used on a simple import"),
        }
    }
}
