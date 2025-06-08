#[derive(Debug, Clone)]
pub enum Token {
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
    ColonEquals,
    Underscore,
    OpenBracket,
    CloseBracket,
    Hash,
    At,
    Comma,

    // Literals
    Identifier(String),
    StringLiteral(String),
    Number(String),

    // Keywords
    Let,
    Fn,
    On,
    Include,
    Decl,

    // Special
    Eof,
    Unknown(char),
}
