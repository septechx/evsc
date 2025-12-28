use lazy_static::lazy_static;
use std::{collections::HashMap, fmt::Display};

use crate::errors::SourceLocation;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: SourceLocation,
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
        m
    };
}

impl Token {
    pub fn lookup_reserved(ident: &str) -> Option<TokenKind> {
        RESERVED_KEYWORDS.get(ident).cloned()
    }
}
