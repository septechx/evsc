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

    // Special
    Eof,
    Unknown,
}
