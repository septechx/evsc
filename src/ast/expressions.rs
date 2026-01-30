use thin_vec::ThinVec;

use crate::{
    ast::{Block, Expr, Ident, Type},
    hashmap::FxHashMap,
    lexer::token::Token,
};

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct SymbolExpr {
    pub value: Ident,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct PostfixExpr {
    pub left: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct PrefixExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr {
    pub assigne: Box<Expr>,
    pub operator: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructInstantiationExpr {
    pub name: Ident,
    pub fields: FxHashMap<Ident, Expr>,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteralExpr {
    pub underlying: Type,
    pub contents: ThinVec<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpr {
    pub callee: Box<Expr>,
    pub parameters: ThinVec<Expr>,
}

#[derive(Debug, Clone)]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub member: Ident,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct TypeExpr {
    pub underlying: Type,
}

#[derive(Debug, Clone)]
pub struct AsExpr {
    pub expr: Box<Expr>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TupleLiteralExpr {
    pub elements: ThinVec<Expr>,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then_branch: Block,
    pub else_branch: Option<Box<Expr>>, // can be another if-expression or a block expression
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
    pub condition: Box<Expr>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct BreakExpr {
    pub value: Option<Box<Expr>>,
}
