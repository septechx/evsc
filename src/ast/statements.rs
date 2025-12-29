use crate::{
    ast::{Attribute, Expression, Statement, Type},
    errors::SourceLocation,
};

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
    pub is_static: bool,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct StructProperty {
    pub name: String,
    pub explicit_type: Type,
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
    pub location: SourceLocation,
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
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct FnArgument {
    pub name: String,
    pub explicit_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct FnDeclStmt {
    pub name: String,
    pub arguments: Vec<FnArgument>,
    pub body: Vec<Statement>,
    pub return_type: Type,
    pub is_public: bool,
    pub is_extern: bool,
    pub attributes: Vec<Attribute>,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expression>,
    pub location: SourceLocation,
}
