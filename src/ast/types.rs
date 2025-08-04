use super::ast::Type;

#[derive(Debug, Clone)]
pub struct SymbolType {
    pub name: String,
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
pub struct ConstType {
    pub underlying: Box<Type>,
}
