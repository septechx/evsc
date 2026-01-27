use thin_vec::ThinVec;

use crate::ast::{Block, Expr, Ident, ImportTree, Mutability, Type, Visibility};

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expr,
    pub has_semicolon: bool,
}

#[derive(Debug, Clone)]
pub struct VarDeclStmt {
    pub variable_name: Ident,
    pub assigned_value: Option<Expr>,
    pub ty: Type,
    pub is_static: bool,
    pub mutability: Mutability,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Ident,
    pub ty: Type,
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
    pub fields: ThinVec<StructField>,
    pub methods: ThinVec<StructMethod>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct ImplStmt {
    pub self_ty: Type,
    pub interface: Ident,
    pub items: ThinVec<InterfaceMethod>,
}

#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    pub fn_decl: FnDeclStmt,
}

#[derive(Debug, Clone)]
pub struct InterfaceDeclStmt {
    pub name: Ident,
    pub methods: ThinVec<InterfaceMethod>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct FnArgument {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FnDeclStmt {
    pub name: Ident,
    pub arguments: ThinVec<FnArgument>,
    pub body: Option<Block>,
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
