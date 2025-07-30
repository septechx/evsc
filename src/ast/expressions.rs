use super::ast::{Expression, Type};
use crate::lexer::token::Token;
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
    pub operator: Token,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub operator: Token,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub assigne: Box<Expression>,
    pub operator: Token,
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
