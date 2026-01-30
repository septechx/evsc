use thin_vec::ThinVec;

use crate::ast::{Block, Expr, Ident, ImportTree, Mutability, Type, Visibility};

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct SemiStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub variable_name: Ident,
    pub assigned_value: Option<Expr>,
    pub ty: Type,
    pub mutability: Mutability,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Static {
    pub variable_name: Ident,
    pub assigned_value: Option<Expr>,
    pub ty: Type,
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
    pub fn_decl: Fn,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Ident,
    pub fields: ThinVec<StructField>,
    pub methods: ThinVec<StructMethod>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub self_ty: Type,
    pub interface: Ident,
    pub items: ThinVec<InterfaceMethod>,
}

#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    pub fn_decl: Fn,
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub name: Ident,
    pub methods: ThinVec<InterfaceMethod>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct FnParameter {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: Ident,
    pub parameters: ThinVec<FnParameter>,
    pub body: Option<Block>,
    pub return_type: Type,
    pub is_extern: bool,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub tree: ImportTree,
    pub visibility: Visibility,
}
