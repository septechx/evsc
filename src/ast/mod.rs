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
    pub name: String,
    pub arguments: Option<Vec<String>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub id: NodeId,
    pub span: Span,
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
    Prefix(PrefixExpr),
    Assignment(AssignmentExpr),
    StructInstantiation(StructInstantiationExpr),
    ArrayLiteral(ArrayLiteralExpr),
    FixedArrayLiteral(FixedArrayLiteralExpr),
    FunctionCall(FunctionCallExpr),
    MemberAccess(MemberAccessExpr),
    Type(TypeExpr),
}

#[derive(Debug, Clone)]
pub enum Type {
    Symbol(SymbolType),
    Pointer(PointerType),
    Slice(SliceType),
    FixedArray(FixedArrayType),
    Mut(MutType),
    Function(FunctionType),
}
