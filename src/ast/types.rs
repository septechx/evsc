use crate::ast::Type;

#[derive(Debug, Clone)]
pub struct SymbolType {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct PointerType {
    pub underlying: Box<Type>,
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
pub struct MutType {
    pub underlying: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,
}
