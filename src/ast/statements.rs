use crate::ast::{Expr, Ident, ImportTree, Mutability, Type, Visibility};

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expr,
    pub has_semicolon: bool,
}

#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    pub variable_name: Ident,
    pub assigned_value: Option<Expr>,
    pub type_: Type,
    pub is_static: bool,
    pub mutability: Mutability,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct StructProperty {
    pub name: Ident,
    pub type_: Type,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct StructMethod {
    pub is_static: bool,
    pub fn_decl: FnDeclStmt,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct StructDeclStmt {
    pub name: Ident,
    pub properties: Box<[StructProperty]>,
    pub methods: Box<[StructMethod]>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    pub fn_decl: FnDeclStmt,
}

#[derive(Debug, Clone)]
pub struct InterfaceDeclStmt {
    pub name: Ident,
    pub methods: Box<[InterfaceMethod]>,
    pub visibility: Visibility,
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
    pub is_extern: bool,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub tree: ImportTree,
    pub visibility: Visibility,
}
