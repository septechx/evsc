pub mod expressions;
pub mod statements;
pub mod types;

use crate::{
    ast::{expressions::*, statements::*, types::*},
    errors::SourceLocation,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute {
    pub name: String,
    pub arguments: Option<Vec<String>>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Block(BlockStmt),
    Expression(ExpressionStmt),
    VarDecl(VarDeclStmt),
    StructDecl(StructDeclStmt),
    InterfaceDecl(InterfaceDeclStmt),
    FnDecl(FnDeclStmt),
    Return(ReturnStmt),
}

#[derive(Debug, Clone)]
pub enum Expression {
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
