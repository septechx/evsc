pub mod display;
pub mod expressions;
pub mod statements;
pub mod types;
pub mod visit;

use anyhow::bail;

use crate::{
    ast::{display::DisplayContext, expressions::*, statements::*, types::*},
    lexer::token::{Token, TokenKind},
    span::Span,
};

#[derive(Debug, Clone)]
pub struct Ast(pub Box<[Stmt]>);

impl Ast {
    pub fn display(&self, color: bool) -> Result<String, std::fmt::Error> {
        let ctx = DisplayContext::new(color);
        let mut output = String::new();
        for (i, stmt) in self.0.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }
            display::write_stmt(&mut output, stmt, &ctx)?;
        }
        Ok(output)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute {
    pub name: Ident,
    pub arguments: Option<Box<[Box<str>]>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub attributes: Box<[Attribute]>,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Expression(ExpressionStmt),
    VarDecl(VarDeclStmt),
    StructDecl(StructDeclStmt),
    InterfaceDecl(InterfaceDeclStmt),
    FnDecl(FnDeclStmt),
    Return(ReturnStmt),
    Import(ImportStmt),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(NumberExpr),
    String(StringExpr),
    Symbol(SymbolExpr),
    Binary(BinaryExpr),
    Postfix(PostfixExpr),
    Prefix(PrefixExpr),
    Assignment(AssignmentExpr),
    StructInstantiation(StructInstantiationExpr),
    ArrayLiteral(ArrayLiteralExpr),
    FunctionCall(FunctionCallExpr),
    MemberAccess(MemberAccessExpr),
    Type(TypeExpr),
    As(AsExpr),
    TupleLiteral(TupleLiteralExpr),
    Block(BlockExpr),
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
    Mut(MutType),
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

#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    pub segments: Box<[Ident]>,
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
        items: Box<[ImportTree]>,
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
    pub fn ident(&self) -> Ident {
        match &self.kind {
            ImportTreeKind::Simple(Some(rename)) => rename.clone(),
            ImportTreeKind::Simple(None) => self
                .prefix
                .segments
                .last()
                .expect("empty prefix in a simple import")
                .clone(),
            _ => panic!("`UseTree::ident` can only be used on a simple import"),
        }
    }
}
