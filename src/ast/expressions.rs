use super::ast::Expr;
use crate::lexer::token::Token;
use num::Num;
use std::clone::Clone;

// Literals
#[derive(Debug)]
pub struct NumberExpr<T: Num + Clone + std::fmt::Debug + 'static> {
    pub value: T,
}

impl<T: Num + Clone + std::fmt::Debug + 'static> Expr for NumberExpr<T> {
    fn expr(&self) -> () {}

    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(NumberExpr {
            value: self.value.clone(),
        })
    }
}

#[derive(Debug)]
pub struct StringExpr {
    pub value: String,
}

impl Expr for StringExpr {
    fn expr(&self) -> () {}

    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(StringExpr {
            value: self.value.clone(),
        })
    }
}

#[derive(Debug)]
pub struct SymbolExpr {
    pub value: String,
}

impl Expr for SymbolExpr {
    fn expr(&self) -> () {}

    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(SymbolExpr {
            value: self.value.clone(),
        })
    }
}

// Complex
#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<dyn Expr>,
    pub operator: Token,
    pub right: Box<dyn Expr>,
}

impl Expr for BinaryExpr {
    fn expr(&self) -> () {}

    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(BinaryExpr {
            left: self.left.clone_box(),
            operator: self.operator.clone(),
            right: self.right.clone_box(),
        })
    }
}

#[derive(Debug)]
pub struct PrefixExpr {
    pub operator: Token,
    pub right: Box<dyn Expr>,
}

impl Expr for PrefixExpr {
    fn expr(&self) -> () {}

    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(PrefixExpr {
            operator: self.operator.clone(),
            right: self.right.clone_box(),
        })
    }
}

#[derive(Debug)]
pub struct AssignmentExpr {
    pub assigne: Box<dyn Expr>,
    pub operator: Token,
    pub value: Box<dyn Expr>,
}

impl Expr for AssignmentExpr {
    fn expr(&self) -> () {}

    fn clone_box(&self) -> Box<dyn Expr> {
        Box::new(AssignmentExpr {
            assigne: self.assigne.clone_box(),
            operator: self.operator.clone(),
            value: self.value.clone_box(),
        })
    }
}

impl Clone for Box<dyn Expr> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}
