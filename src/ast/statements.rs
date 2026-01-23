use crate::ast::{Expr, Ident, ImportTree, Type};

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    pub variable_name: Ident,
    pub is_constant: bool,
    pub is_public: bool,
    pub assigned_value: Option<Expr>,
    pub type_: Type,
    pub is_static: bool,
}

#[derive(Debug, Clone)]
pub struct StructProperty {
    pub name: Ident,
    pub type_: Type,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct StructMethod {
    pub is_static: bool,
    pub fn_decl: FnDeclStmt,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct StructDeclStmt {
    pub name: Ident,
    pub properties: Box<[StructProperty]>,
    pub methods: Box<[StructMethod]>,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    pub fn_decl: FnDeclStmt,
}

#[derive(Debug, Clone)]
pub struct InterfaceDeclStmt {
    pub name: Ident,
    pub methods: Box<[InterfaceMethod]>,
    pub is_public: bool,
}

#[derive(Debug, Clone)]
pub struct FnArgument {
    pub name: Ident,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct FnDeclStmt {
    pub name: Ident,
    pub arguments: Box<[FnArgument]>,
    pub body: Option<Expr>,
    pub return_type: Type,
    pub is_public: bool,
    pub is_extern: bool,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub tree: ImportTree,
}
