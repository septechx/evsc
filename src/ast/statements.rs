use super::ast::{Expr, Stmt};

#[derive(Debug)]
pub struct BlockStmt {
    pub body: Vec<Box<dyn Stmt>>,
}

impl Stmt for BlockStmt {
    fn stmt(&self) -> () {}
}

#[derive(Debug)]
pub struct ExpressionStmt {
    pub expression: Box<dyn Expr>,
}

impl Stmt for ExpressionStmt {
    fn stmt(&self) -> () {}
}

#[derive(Debug)]
pub struct VarDeclStmt {
    pub variable_name: String,
    pub is_constant: bool,
    pub assigned_value: Box<dyn Expr>,
}

impl Stmt for VarDeclStmt {
    fn stmt(&self) -> () {}
}
