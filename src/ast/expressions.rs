use super::ast::{Expr, Type};
use crate::lexer::token::Token;
use num::Num;
use std::{any::Any, clone::Clone, collections::HashMap};

// Literals
#[derive(Debug, Clone)]
pub struct NumberExpr<T: Num + Clone + std::fmt::Debug + 'static> {
    pub value: T,
}

impl<T: Num + Clone + std::fmt::Debug + 'static> Expr for NumberExpr<T> {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(NumberExpr {
            value: self.value.clone(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct StringExpr {
    pub value: String,
}

impl Expr for StringExpr {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(StringExpr {
            value: self.value.clone(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct SymbolExpr {
    pub value: String,
}

impl Expr for SymbolExpr {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(SymbolExpr {
            value: self.value.clone(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

// Complex
#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<dyn Expr>,
    pub operator: Token,
    pub right: Box<dyn Expr>,
}

impl Expr for BinaryExpr {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(BinaryExpr {
            left: self.left.clone_box(),
            operator: self.operator.clone(),
            right: self.right.clone_box(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub operator: Token,
    pub right: Box<dyn Expr>,
}

impl Expr for PrefixExpr {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(PrefixExpr {
            operator: self.operator.clone(),
            right: self.right.clone_box(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub assigne: Box<dyn Expr>,
    pub operator: Token,
    pub value: Box<dyn Expr>,
}

impl Expr for AssignmentExpr {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(AssignmentExpr {
            assigne: self.assigne.clone_box(),
            operator: self.operator.clone(),
            value: self.value.clone_box(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct StructInstantiationExpr {
    pub name: String,
    pub properties: HashMap<String, Box<dyn Expr>>,
}

impl Expr for StructInstantiationExpr {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(StructInstantiationExpr {
            name: self.name.clone(),
            properties: self.properties.clone(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub underlying: Box<dyn Type>,
    pub contents: Vec<Box<dyn Expr>>,
}

impl Expr for ArrayLiteralExpr {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(ArrayLiteralExpr {
            underlying: self.underlying.clone(),
            contents: self.contents.clone(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct FixedArrayLiteralExpr {
    pub underlying: Box<dyn Type>,
    pub length: usize,
    pub contents: Vec<Box<dyn Expr>>,
}

impl Expr for FixedArrayLiteralExpr {
    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(FixedArrayLiteralExpr {
            underlying: self.underlying.clone(),
            length: self.length,
            contents: self.contents.clone(),
        })
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Clone for Box<dyn Expr> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}
