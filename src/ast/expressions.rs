use crate::{
    ast::{Expr, Ident, Type},
    lexer::token::Token,
};
use std::collections::HashMap;

// TODO: Add float support, replace i32
#[derive(Debug, Clone)]
pub struct NumberExpr {
    pub value: i32,
}

#[derive(Debug, Clone)]
pub struct StringExpr {
    pub value: Box<str>,
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
    pub properties: HashMap<Box<str>, Expr>,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub underlying: Type,
    pub contents: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpr {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub member: Ident,
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
