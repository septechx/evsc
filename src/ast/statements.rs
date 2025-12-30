use crate::ast::{Attribute, Expr, Stmt, Type};

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    pub variable_name: String,
    pub is_constant: bool,
    pub assigned_value: Option<Expr>,
    pub type_: Type,
    pub is_static: bool,
}

#[derive(Debug, Clone)]
pub struct StructProperty {
    pub name: String,
    pub type_: Type,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct StructMethod {
    pub is_static: bool,
    pub fn_decl: FnDeclStmt,
}

#[derive(Debug, Clone)]
pub struct StructDeclStmt {
    pub name: String,
    pub properties: Vec<StructProperty>,
    pub methods: Vec<StructMethod>,
    pub is_public: bool,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    pub fn_decl: FnDeclStmt,
}

#[derive(Debug, Clone)]
pub struct InterfaceDeclStmt {
    pub name: String,
    pub methods: Vec<InterfaceMethod>,
    pub is_public: bool,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct FnArgument {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct FnDeclStmt {
    pub name: String,
    pub arguments: Vec<FnArgument>,
    pub body: Vec<Stmt>,
    pub return_type: Type,
    pub is_public: bool,
    pub is_extern: bool,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}
