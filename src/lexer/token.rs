use lazy_static::lazy_static;
use std::{collections::HashMap, mem};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token {
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
    Identifier(String),
    StringLiteral(String),
    Number(i32),

    // Reserved
    Let,
    True,
    False,
    Struct,
    Fn,
    Return,
    Const,
    Pub,
    Static,
    Mut,

    // Special
    Eof,
}

use Token as T;
lazy_static! {
    static ref RESERVED_KEYWORDS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("true", T::True);
        m.insert("false", T::False);
        m.insert("let", T::Let);
        m.insert("struct", T::Struct);
        m.insert("fn", T::Fn);
        m.insert("return", T::Return);
        m.insert("const", T::Const);
        m.insert("pub", T::Pub);
        m.insert("static", T::Static);
        m.insert("mut", T::Mut);
        m
    };
}

impl Token {
    pub fn lookup_reserved(ident: &str) -> Option<Token> {
        RESERVED_KEYWORDS.get(ident).cloned()
    }

    pub fn unwrap_number(self) -> i32 {
        match self {
            Token::Number(value) => value,
            _ => panic!("Expected number, got {self:?}"),
        }
    }

    pub fn unwrap_string(self) -> String {
        match self {
            Token::StringLiteral(value) => value,
            _ => panic!("Expected string, got {self:?}"),
        }
    }

    pub fn unwrap_identifier(self) -> String {
        match self {
            Token::Identifier(value) => value,
            _ => panic!("Expected identifier, got {self:?}"),
        }
    }

    pub fn eq(&self, other: &Token) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }

    pub fn identifier() -> Token {
        Token::Identifier("".to_string())
    }

    pub fn string_literal() -> Token {
        Token::StringLiteral("".to_string())
    }

    pub fn number() -> Token {
        Token::Number(0)
    }
}
