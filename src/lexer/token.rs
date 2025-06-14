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

// We need a way to match tokens without their data for lookup purposes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Symbols
    Semicolon,
    Pipe,
    Colon,
    Arrow,
    OpenCurly,
    CloseCurly,
    DoubleColon,
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
    At,
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

    // Literals
    Identifier,
    StringLiteral,
    Number,

    // Reserved
    Const,
    Let,
    True,
    False,
    Null,
    Struct,
    Static,

    // Special
    Eof,
}

lazy_static! {
    static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut m = HashMap::new();
        m.insert("true", TokenKind::True);
        m.insert("false", TokenKind::False);
        m.insert("null", TokenKind::Null);
        m.insert("let", TokenKind::Let);
        m.insert("const", TokenKind::Const);
        m.insert("struct", TokenKind::Struct);
        m.insert("static", TokenKind::Static);
        m
    };
}

impl TokenKind {
    pub fn lookup_reserved(ident: &str) -> Option<TokenKind> {
        RESERVED_KEYWORDS.get(ident).copied()
    }
}
