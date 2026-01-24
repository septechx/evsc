use std::collections::HashMap;

use crate::{
    ast::{Ast, Literal},
    hir::{
        interner::{Interner, Symbol},
        lower::LoweringContext,
    },
};

mod interner;
mod lower;

pub fn lower_ast(asts: Vec<Ast>) -> HirCrate {
    let mut ctx = LoweringContext::new();
    ctx.lower_crate(asts);
    ctx.krate
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

impl From<u32> for DefId {
    fn from(v: u32) -> Self {
        DefId(v)
    }
}
impl From<u32> for ExprId {
    fn from(v: u32) -> Self {
        ExprId(v)
    }
}
impl From<u32> for TypeId {
    fn from(v: u32) -> Self {
        TypeId(v)
    }
}
impl From<u32> for LocalId {
    fn from(v: u32) -> Self {
        LocalId(v)
    }
}
impl From<u32> for StmtId {
    fn from(v: u32) -> Self {
        StmtId(v)
    }
}
impl From<u32> for ModuleId {
    fn from(v: u32) -> Self {
        ModuleId(v)
    }
}

#[derive(Debug, Clone)]
pub struct HirCrate {
    pub modules: Vec<ModuleInfo>,
    pub defs: Vec<Def>,
    pub exprs: Vec<HirExpr>,
    pub types: Vec<HirType>,
    pub stmts: Vec<HirStmt>,
    pub interner: Interner,
    pub diagnostics: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub exports: HashMap<Symbol, DefId>,
    pub items: Vec<DefId>,
    pub imports: Vec<ImportEntry>,
}

#[derive(Debug, Clone)]
pub struct ImportEntry {
    pub name: Symbol,
    pub def: DefId,
}

#[derive(Debug, Clone)]
pub enum Def {
    Placeholder(DefId),
    Function(Function),
    Struct(Struct),
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<(Symbol, TypeId)>,
    pub ret: TypeId,
    pub body: Option<ExprId>,
    pub module: ModuleId,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Symbol,
    pub fields: Vec<(Symbol, TypeId)>,
    pub module: ModuleId,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Symbol,
    pub ty: Option<TypeId>,
    pub init: Option<ExprId>,
    pub module: ModuleId,
}

#[derive(Debug, Clone)]
pub enum HirExpr {
    Error,
    Literal(Literal),
    Local(LocalId),
    Global(DefId),
    Call {
        callee: ExprId,
        args: Vec<ExprId>,
    },
    Field {
        base: ExprId,
        field: Symbol,
    },
    StructInit {
        def: DefId,
        fields: Vec<(Symbol, ExprId)>,
    },
    Block {
        stmts: Vec<StmtId>,
    },
}

#[derive(Debug, Clone)]
pub enum HirStmt {
    Expr(ExprId),
    Let {
        name: Symbol,
        ty: Option<TypeId>,
        init: ExprId,
        local: LocalId,
    },
    Return(Option<ExprId>),
}

#[derive(Debug, Clone)]
pub enum HirType {
    Error,
    Builtin(String),
    Adt(DefId),
}
