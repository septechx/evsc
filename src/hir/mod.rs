use thin_vec::ThinVec;

use crate::ast::{Mutability, Visibility};
use crate::hashmap::FxHashMap;

use crate::{
    ast::{Ast, Literal},
    hir::{
        interner::{Interner, Symbol},
        lower::LoweringContext,
    },
    lexer::token::TokenKind,
};

mod interner;
mod lower;
mod resolve;

pub fn lower_ast(asts: ThinVec<Ast>) -> HirCrate {
    let mut ctx = LoweringContext::new();
    ctx.lower_crate(asts);
    for diag in &ctx.krate.diagnostics {
        crate::error!(diag.clone());
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyId(pub u32);

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
impl From<u32> for BodyId {
    fn from(v: u32) -> Self {
        BodyId(v)
    }
}

#[derive(Debug, Default)]
pub struct HirCrate {
    pub modules: ThinVec<ModuleInfo>,
    pub defs: ThinVec<Def>,
    pub exprs: ThinVec<HirExpr>,
    pub types: ThinVec<HirType>,
    pub stmts: ThinVec<HirStmt>,
    pub bodies: ThinVec<Body>,
    pub interner: Interner,
    pub diagnostics: ThinVec<String>,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub exports: FxHashMap<Symbol, ExportEntry>,
    pub items: ThinVec<DefId>,
    pub imports: FxHashMap<Symbol, DefId>,
    pub struct_methods: FxHashMap<DefId, FxHashMap<Symbol, MethodMeta>>,
    pub struct_fields: FxHashMap<DefId, FxHashMap<Symbol, Visibility>>,
    pub struct_impls: FxHashMap<DefId, ThinVec<DefId>>,
}

#[derive(Debug, Clone)]
pub struct ExportEntry {
    pub def: DefId,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub enum Def {
    Placeholder(DefId, ModuleId),
    Function(Function),
    Struct(Struct),
    Interface(Interface),
    Variable(Variable),
}

impl Def {
    pub fn module(&self) -> ModuleId {
        match self {
            Def::Placeholder(_, m) => *m,
            Def::Function(f) => f.module,
            Def::Struct(s) => s.module,
            Def::Interface(i) => i.module,
            Def::Variable(v) => v.module,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    pub params: ThinVec<(Symbol, TypeId)>,
    pub ret: TypeId,
    pub body: Option<BodyId>,
    pub module: ModuleId,
    /// struct defid if this is a method
    pub associated: Option<DefId>,
    pub static_method: bool,
}

#[derive(Debug, Clone)]
pub struct Body {
    pub stmts: ThinVec<StmtId>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Symbol,
    pub ty: TypeId,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Symbol,
    pub fields: ThinVec<StructField>,
    pub methods: ThinVec<(Symbol, DefId)>,
    pub module: ModuleId,
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub name: Symbol,
    pub methods: ThinVec<InterfaceMethod>,
    pub module: ModuleId,
}

#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    pub name: Symbol,
    pub params: ThinVec<TypeId>,
    pub ret: TypeId,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Symbol,
    pub ty: Option<TypeId>,
    pub init: Option<ExprId>,
    pub module: ModuleId,
}

#[derive(Debug, Clone)]
pub struct MethodMeta {
    pub def: DefId,
    pub is_static: bool,
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub enum HirExpr {
    Error,
    Literal(Literal),
    Local(LocalId),
    Global(DefId),
    Call {
        callee: ExprId,
        args: ThinVec<ExprId>,
    },
    MethodCall {
        base: ExprId,
        method: Symbol,
        args: ThinVec<ExprId>,
    },
    Field {
        base: ExprId,
        field: Symbol,
    },
    StructInit {
        def: DefId,
        fields: ThinVec<(Symbol, ExprId)>,
    },
    Block {
        stmts: ThinVec<StmtId>,
    },
    Binary {
        left: ExprId,
        op: BinOp,
        right: ExprId,
    },
}

#[derive(Debug, Clone)]
pub enum HirStmt {
    /// Expression without a trailing semicolon
    Expr(ExprId),
    /// Expression with a trailing semicolon
    Semi(ExprId),
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
    Pointer(TypeId, Mutability),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Math
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
    // Comparisons
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical
    And,
    Or,
}

impl From<TokenKind> for BinOp {
    fn from(value: TokenKind) -> Self {
        use BinOp as B;
        use TokenKind as T;
        match value {
            T::Plus => B::Add,
            T::Dash => B::Sub,
            T::Star => B::Mul,
            T::Slash => B::Div,
            T::Percent => B::Rem,
            T::ShiftLeft => B::Shl,
            T::ShiftRight => B::Shr,
            T::Reference => B::BitAnd,
            T::Bar => B::BitOr,
            T::Xor => B::BitXor,
            T::EqualsEquals => B::Eq,
            T::NotEquals => B::Ne,
            T::Less => B::Lt,
            T::LessEquals => B::Le,
            T::More => B::Gt,
            T::MoreEquals => B::Ge,
            T::And => B::And,
            T::Or => B::Or,
            _ => panic!("Cannot convert token {} to BinOp", value),
        }
    }
}
