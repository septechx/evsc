use super::ast::Type;

#[derive(Debug)]
pub struct SymbolType {
    pub name: String,
}

impl Type for SymbolType {
    fn clone_box(&self) -> Box<dyn Type> {
        Box::new(SymbolType {
            name: self.name.clone(),
        })
    }
}

#[derive(Debug)]
pub struct VectorType {
    pub underlying: Box<dyn Type>,
}

impl Type for VectorType {
    fn clone_box(&self) -> Box<dyn Type> {
        Box::new(VectorType {
            underlying: self.underlying.clone_box(),
        })
    }
}

impl Clone for Box<dyn Type> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}
