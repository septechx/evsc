use super::expressions::*;
use super::statements::*;
use super::types::*;

#[derive(Debug, Clone)]
pub enum Statement {
    Block(BlockStmt),
    Expression(ExpressionStmt),
    VarDecl(VarDeclStmt),
    StructDecl(StructDeclStmt),
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
}

#[derive(Debug, Clone)]
pub enum Type {
    Symbol(SymbolType),
    Slice(SliceType),
    FixedArray(FixedArrayType),
    Const(ConstType),
}
