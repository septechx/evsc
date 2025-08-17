use lazy_static::lazy_static;
use std::{collections::HashMap, mem};

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
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.eq(other)
    }
}

impl Eq for Token {}

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
