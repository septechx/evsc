use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use crate::span::{ModuleId, Span};

#[derive(Debug, Clone)]
pub struct TokenStream(pub Vec<Token>);

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub module_id: ModuleId,
    pub value: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Token {}

#[derive(Debug, Clone, PartialOrd, Ord, Hash, Eq, PartialEq, Copy)]
pub enum TokenKind {
    // Symbols
    Semicolon,
    Pipe,
    Colon,
    Arrow,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Dot,
    Equals,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    PercentEquals,
    Underscore,
    OpenBracket,
    CloseBracket,
    Hash,
    Comma,
    Plus,
    Dash,
    Star,
    Slash,
    Percent,
    And,
    Or,
    DotDot,
    EqualsEquals,
    NotEquals,
    Less,
    More,
    LessEquals,
    MoreEquals,
    Reference,
    Dollar,

    // Literals
    Identifier,
    StringLiteral,
    Number,

    // Reserved
    Let,
    True,
    False,
    Struct,
    Fn,
    Return,
    Pub,
    Static,
    Mut,
    Extern,
    Interface,

    // Special
    Eof,
    Illegal,
}

use TokenKind as T;
lazy_static! {
    static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut m = HashMap::new();
        m.insert("true", T::True);
        m.insert("false", T::False);
        m.insert("let", T::Let);
        m.insert("struct", T::Struct);
        m.insert("fn", T::Fn);
        m.insert("return", T::Return);
        m.insert("pub", T::Pub);
        m.insert("static", T::Static);
        m.insert("mut", T::Mut);
        m.insert("extern", T::Extern);
        m.insert("interface", T::Interface);
        m
    };
}

impl Token {
    pub fn lookup_reserved(ident: &str) -> Option<TokenKind> {
        RESERVED_KEYWORDS.get(ident).cloned()
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            T::Let => write!(f, "let"),
            T::True => write!(f, "true"),
            T::False => write!(f, "false"),
            T::Struct => write!(f, "struct"),
            T::Fn => write!(f, "fn"),
            T::Return => write!(f, "return"),
            T::Pub => write!(f, "pub"),
            T::Static => write!(f, "static"),
            T::Mut => write!(f, "mut"),
            T::Extern => write!(f, "extern"),
            T::Interface => write!(f, "interface"),
            T::Eof => write!(f, "eof"),
            T::Illegal => write!(f, "illegal"),
            T::Identifier => write!(f, "identifier"),
            T::StringLiteral => write!(f, "string literal"),
            T::Number => write!(f, "number"),
            T::Semicolon => write!(f, ";"),
            T::Pipe => write!(f, "|"),
            T::Colon => write!(f, ":"),
            T::Arrow => write!(f, "->"),
            T::OpenCurly => write!(f, "{{"),
            T::CloseCurly => write!(f, "}}"),
            T::OpenParen => write!(f, "("),
            T::CloseParen => write!(f, ")"),
            T::Dot => write!(f, "."),
            T::Equals => write!(f, "="),
            T::PlusEquals => write!(f, "+="),
            T::MinusEquals => write!(f, "-="),
            T::StarEquals => write!(f, "*="),
            T::SlashEquals => write!(f, "/="),
            T::PercentEquals => write!(f, "%="),
            T::Underscore => write!(f, "_"),
            T::OpenBracket => write!(f, "["),
            T::CloseBracket => write!(f, "]"),
            T::Hash => write!(f, "#"),
            T::Comma => write!(f, ","),
            T::Plus => write!(f, "+"),
            T::Dash => write!(f, "-"),
            T::Star => write!(f, "*"),
            T::Slash => write!(f, "/"),
            T::Percent => write!(f, "%"),
            T::And => write!(f, "&"),
            T::Or => write!(f, "|"),
            T::DotDot => write!(f, ".."),
            T::EqualsEquals => write!(f, "=="),
            T::NotEquals => write!(f, "!="),
            T::Less => write!(f, "<"),
            T::More => write!(f, ">"),
            T::LessEquals => write!(f, "<="),
            T::MoreEquals => write!(f, ">="),
            T::Reference => write!(f, "@"),
            T::Dollar => write!(f, "$"),
        }
    }
}
