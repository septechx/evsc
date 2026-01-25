use crate::{
    ast::{Expr, Ident, Stmt, Type},
    lexer::token::Token,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub body: Box<[Stmt]>,
}

#[derive(Debug, Clone)]
pub struct SymbolExpr {
    pub value: Ident,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct PostfixExpr {
    pub left: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub assigne: Box<Expr>,
    pub operator: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructInstantiationExpr {
    pub name: Ident,
    pub properties: HashMap<Ident, Expr>,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub underlying: Type,
    pub contents: Box<[Expr]>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpr {
    pub callee: Box<Expr>,
    pub arguments: Box<[Expr]>,
}

#[derive(Debug, Clone)]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub member: Ident,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct TypeExpr {
    pub underlying: Type,
}

#[derive(Debug, Clone)]
pub struct AsExpr {
    pub expr: Box<Expr>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TupleLiteralExpr {
    pub elements: Box<[Expr]>,
}
