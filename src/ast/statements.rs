use super::ast::{Expression, Statement, Type};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    pub variable_name: String,
    pub is_constant: bool,
    pub assigned_value: Option<Expression>,
    pub explicit_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct StructProperty {
    pub is_static: bool,
    pub explicit_type: Type,
}

#[derive(Debug, Clone)]
pub struct StructMethod {
    pub is_static: bool,
}

#[derive(Debug, Clone)]
pub struct StructDeclStmt {
    pub name: String,
    pub properties: HashMap<String, StructProperty>,
    pub methods: HashMap<String, StructMethod>,
}

#[derive(Debug, Clone)]
pub struct FnArgument {
    pub name: String,
    pub explicit_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct FnDeclStmt {
    pub name: String,
    pub arguments: HashMap<String, FnArgument>,
    pub body: Vec<Statement>,
    pub explicit_type: Type,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expression>,
}
