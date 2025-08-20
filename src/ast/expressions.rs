use crate::{
    ast::ast::{Expression, Type},
    lexer::token::LocatedToken,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct NumberExpr {
    pub value: i32,
}

#[derive(Debug, Clone)]
pub struct StringExpr {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct SymbolExpr {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expression>,
    pub operator: LocatedToken,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub operator: LocatedToken,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub assigne: Box<Expression>,
    pub operator: LocatedToken,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct StructInstantiationExpr {
    pub name: String,
    pub properties: HashMap<String, Expression>,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub underlying: Type,
    pub contents: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct FixedArrayLiteralExpr {
    pub underlying: Type,
    pub length: usize,
    pub contents: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpr {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct MemberAccessExpr {
    pub base: Box<Expression>,
    pub member: SymbolExpr,
}
