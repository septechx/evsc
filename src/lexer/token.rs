use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub value: String,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind, value: String) -> Self {
        Self { value, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    Const,
    Pub,

    // Special
    Eof,
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
        m.insert("const", T::Const);
        m.insert("pub", T::Pub);
        m
    };
}

impl TokenKind {
    pub fn lookup_reserved(ident: &str) -> Option<TokenKind> {
        RESERVED_KEYWORDS.get(ident).copied()
    }
}
