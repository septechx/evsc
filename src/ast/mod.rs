pub mod display;
pub mod types;
pub mod validate;
pub mod visit;

use anyhow::bail;
use thin_vec::ThinVec;

use crate::{
    ast::{display::DisplayContext, types::*},
    lexer::token::{Token, TokenKind},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct Ast {
    pub name: Box<str>,
    pub items: ThinVec<Item>,
}

impl Ast {
    pub fn display(&self, color: bool) -> Result<String, std::fmt::Error> {
        let ctx = DisplayContext::new(color);
        let mut output = String::new();
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }
            display::write_item(&mut output, item, &ctx)?;
        }
        Ok(output)
    }
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
    pub attributes: ThinVec<Attribute>,
    /// Visibility modifier for this item.
    ///
    /// For most item kinds (static, struct, interface, function, import), this is the visibility
    /// as written in the source code (defaults to private if not specified).
    ///
    /// For [`ItemKind::Impl`], this field is a placeholder value since impl blocks do not have
    /// visibility modifiers in the source grammar. The value is always set to [`Visibility::Private`]
    /// for uniformity across the AST. Code that processes items should ignore this field for impls
    /// and instead check the visibility of individual associated items within the impl block.
    pub visibility: Visibility,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Static {
        name: Ident,
        value: Expr,
        ty: Type,
    },
    Struct {
        name: Ident,
        fields: ThinVec<(Ident, Type, Visibility)>,
        items: ThinVec<AssocItem>,
    },
    Interface {
        name: Ident,
        items: ThinVec<AssocItem>,
    },
    Impl {
        self_ty: Type,
        interface: Ident,
        items: ThinVec<AssocItem>,
    },
    Fn(Fn),
    Import(ImportTree),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// Expression without a trailing semicolon (returns value)
    Expr(Expr),
    /// Expression with a trailing semicolon
    Semi(Expr),
    Let {
        name: Ident,
        ty: Type,
        value: Option<Expr>,
        mutability: Mutability,
    },
}

#[derive(Debug, Clone)]
pub struct AssocItem {
    pub kind: AssocItemKind,
    pub visibility: Visibility,
    pub is_static: bool,
}

#[derive(Debug, Clone)]
pub enum AssocItemKind {
    Fn(Fn),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: Ident,
    pub parameters: ThinVec<(Ident, Type)>,
    pub body: Option<Block>,
    pub return_type: Type,
    pub is_extern: bool,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    Symbol(Ident),
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Postfix {
        left: Box<Expr>,
        operator: Token,
    },
    Prefix {
        operator: Token,
        right: Box<Expr>,
    },
    Assignment {
        assignee: Box<Expr>,
        operator: Token,
        value: Box<Expr>,
    },
    StructInstantiation {
        name: Ident,
        fields: ThinVec<(Ident, Expr)>,
    },
    ArrayLiteral {
        underlying: Type,
        contents: ThinVec<Expr>,
    },
    FunctionCall {
        callee: Box<Expr>,
        parameters: ThinVec<Expr>,
    },
    MemberAccess {
        base: Box<Expr>,
        member: Ident,
        operator: Token,
    },
    Type(Type),
    As {
        expr: Box<Expr>,
        ty: Type,
    },
    TupleLiteral {
        elements: ThinVec<Expr>,
    },
    Block(Block),
    If {
        condition: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Box<Expr>>,
    },
    While {
        condition: Box<Expr>,
        body: Block,
    },
    Loop(Block),
    Break(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(Box<str>),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Symbol(SymbolType),
    Pointer(PointerType),
    Slice(SliceType),
    FixedArray(FixedArrayType),
    Function(FunctionType),
    Tuple(TupleType),
    Infer,
    Never,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub value: Box<str>,
    pub span: Span,
}

impl TryFrom<Token> for Ident {
    type Error = anyhow::Error;

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        if token.kind != TokenKind::Identifier {
            bail!("Expected identifier token, but got {} instead", token.kind);
        }
        Ok(Self {
            value: token.value,
            span: token.span,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Constant,
    Mutable,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: ThinVec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    pub segments: ThinVec<Ident>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute {
    pub name: Ident,
    pub parameters: Option<ThinVec<Box<str>>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImportTreeKind {
    /// `import prefix` or `import prefix as rename`
    ///
    /// The inner value represents the rename if it exists.
    Simple(Option<Ident>),
    /// `import prefix::{...}`
    ///
    /// The span represents the braces of the nested group and all elements within:
    ///
    /// ```text
    /// import foo::{bar, baz};
    ///             ^^^^^^^^^^
    /// ```
    Nested {
        items: ThinVec<ImportTree>,
        span: Span,
    },
    /// `import prefix::*`
    Glob,
}

#[derive(Debug, Clone)]
pub struct ImportTree {
    pub prefix: Path,
    pub kind: ImportTreeKind,
    pub span: Span,
}

impl ImportTree {
    pub fn ident(&self) -> Option<Ident> {
        match &self.kind {
            ImportTreeKind::Simple(Some(rename)) => Some(rename.clone()),
            ImportTreeKind::Simple(None) => Some(
                self.prefix
                    .segments
                    .last()
                    .expect("empty prefix in a simple import")
                    .clone(),
            ),
            _ => None,
        }
    }
}
