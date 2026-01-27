use thin_vec::ThinVec;

use crate::ast::{Ident, Mutability, Type};

#[derive(Debug, Clone)]
pub struct SymbolType {
    pub name: Ident,
}

#[derive(Debug, Clone)]
pub struct PointerType {
    pub underlying: Box<Type>,
    pub mutability: Mutability,
}

#[derive(Debug, Clone)]
pub struct SliceType {
    pub underlying: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct FixedArrayType {
    pub length: usize,
    pub underlying: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub parameters: ThinVec<Type>,
    pub return_type: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct TupleType {
    pub elements: ThinVec<Type>,
}
