use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    fmt::Display,
    mem,
    ops::{Deref, DerefMut},
};

use crate::errors::SourceLocation;

#[derive(Debug, Clone, PartialOrd, Ord)]
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
    Illegal(char),
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.eq(other)
    }
}

impl Eq for Token {}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Semicolon => write!(f, ";"),
            Token::Pipe => write!(f, "|"),
            Token::Colon => write!(f, ":"),
            Token::Arrow => write!(f, "->"),
            Token::OpenCurly => write!(f, "{{"),
            Token::CloseCurly => write!(f, "}}"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::Dot => write!(f, "."),
            Token::Equals => write!(f, "="),
            Token::PlusEquals => write!(f, "+="),
            Token::MinusEquals => write!(f, "-="),
            Token::StarEquals => write!(f, "*="),
            Token::SlashEquals => write!(f, "/="),
            Token::PercentEquals => write!(f, "%="),
            Token::Underscore => write!(f, "_"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::Hash => write!(f, "#"),
            Token::Comma => write!(f, ","),
            Token::Plus => write!(f, "+"),
            Token::Dash => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::And => write!(f, "&"),
            Token::Or => write!(f, "|"),
            Token::DotDot => write!(f, ".."),
            Token::EqualsEquals => write!(f, "=="),
            Token::NotEquals => write!(f, "!="),
            Token::Less => write!(f, "<"),
            Token::More => write!(f, ">"),
            Token::LessEquals => write!(f, "<="),
            Token::MoreEquals => write!(f, ">="),
            Token::Reference => write!(f, "@"),
            Token::Identifier(value) => write!(f, "{value}"),
            Token::StringLiteral(value) => write!(f, "\"{value}\""),
            Token::Number(value) => write!(f, "{value}"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Struct => write!(f, "struct"),
            Token::Fn => write!(f, "fn"),
            Token::Return => write!(f, "return"),
            Token::Const => write!(f, "const"),
            Token::Pub => write!(f, "pub"),
            Token::Static => write!(f, "static"),
            Token::Mut => write!(f, "mut"),
            Token::Eof => write!(f, "EOF"),
            Token::Illegal(c) => write!(f, "{c}"),
        }
    }
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

#[derive(Debug, Clone)]
pub struct LocatedToken {
    pub token: Token,
    pub location: SourceLocation,
}

impl LocatedToken {
    pub fn new(token: Token, location: SourceLocation) -> Self {
        Self { token, location }
    }

    pub fn new_simple(token: Token, file: std::path::PathBuf, line: usize, column: usize) -> Self {
        Self {
            token,
            location: SourceLocation::simple(file, line, column),
        }
    }
}

impl Deref for LocatedToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl DerefMut for LocatedToken {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

pub fn extract_tokens(located_tokens: &[LocatedToken]) -> Vec<Token> {
    located_tokens.iter().map(|lt| lt.token.clone()).collect()
}

#[cfg(test)]
mod tests {
    use crate::lexer::token::Token;

    #[test]
    fn test_token_eq() {
        assert!(Token::identifier().eq(&Token::Identifier("".to_string())));
        assert!(Token::string_literal().eq(&Token::StringLiteral("".to_string())));
        assert!(Token::number().eq(&Token::Number(0)));
    }

    #[test]
    fn test_reserved_lookup() {
        assert_eq!(Token::lookup_reserved("true"), Some(Token::True));
        assert_eq!(Token::lookup_reserved("false"), Some(Token::False));
        assert_eq!(Token::lookup_reserved("let"), Some(Token::Let));
        assert_eq!(Token::lookup_reserved("struct"), Some(Token::Struct));
        assert_eq!(Token::lookup_reserved("fn"), Some(Token::Fn));
        assert_eq!(Token::lookup_reserved("return"), Some(Token::Return));
        assert_eq!(Token::lookup_reserved("const"), Some(Token::Const));
        assert_eq!(Token::lookup_reserved("pub"), Some(Token::Pub));
        assert_eq!(Token::lookup_reserved("static"), Some(Token::Static));
        assert_eq!(Token::lookup_reserved("mut"), Some(Token::Mut));
    }
}
