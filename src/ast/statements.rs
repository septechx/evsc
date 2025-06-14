use super::ast::{Expr, Stmt, Type};

#[derive(Debug)]
pub struct BlockStmt {
    pub body: Vec<Box<dyn Stmt>>,
}

impl Stmt for BlockStmt {}

#[derive(Debug)]
pub struct ExpressionStmt {
    pub expression: Box<dyn Expr>,
}

impl Stmt for ExpressionStmt {}

#[derive(Debug)]
pub struct VarDeclStmt {
    pub variable_name: String,
    pub is_constant: bool,
    pub assigned_value: Option<Box<dyn Expr>>,
    pub explicit_type: Option<Box<dyn Type>>,
}

impl Stmt for VarDeclStmt {}
