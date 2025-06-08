use crate::lexer::token::Token;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, Clone)]
pub struct Lexer {
    file_content: String,
    file_len: usize,
    pos: usize,
}

impl Lexer {
    pub fn new(file: String) -> Self {
        Self {
            file_len: file.len(),
            file_content: file,
            pos: 0,
        }
    }

    fn at_eof(&self) -> bool {
        self.pos >= self.file_len
    }

    fn advance(&mut self, len: usize) {
        self.pos += len;
    }

    fn remaining_input(&self) -> &str {
        &self.file_content[self.pos..]
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        while !self.at_eof() {
            let clone = self.clone();
            let remaining = clone.remaining_input();
            let mut matched = false;

            for (regex, token_template) in REGEXES.iter() {
                if let Some(mat) = regex.find(remaining) {
                    if mat.start() == 0 {
                        let matched_text = mat.as_str();

                        if let Some(token_template) = token_template {
                            match token_template {
                                Token::StringLiteral(_) => {
                                    let value = matched_text[1..matched_text.len() - 1].to_string();
                                    tokens.push(Token::StringLiteral(value));
                                }
                                Token::Identifier(_) => {
                                    tokens.push(Token::Identifier(matched_text.to_string()));
                                }
                                Token::Number(_) => {
                                    tokens.push(Token::Number(matched_text.to_string()));
                                }
                                _ => {
                                    tokens.push(token_template.clone());
                                }
                            }
                        }

                        self.advance(matched_text.len());
                        matched = true;
                        break;
                    }
                }
            }

            if !matched {
                panic!(
                    "Unexpected character at position {}: '{}'",
                    self.pos,
                    remaining.chars().next().unwrap_or('\0')
                );
            }
        }

        tokens
    }
}

lazy_static! {
    static ref REGEXES: Vec<(Regex, Option<Token>)> = vec![
        // Whitespace (check first to skip efficiently)
        (Regex::new(r"^\s+").unwrap(), None),

        // Multi-character operators (must come before single chars)
        (Regex::new(r"^::").unwrap(), Some(Token::DoubleColon)),
        (Regex::new(r"^:=").unwrap(), Some(Token::ColonEquals)),
        (Regex::new(r"^->").unwrap(), Some(Token::Arrow)),

        // Keywords (must come before general identifier)
        (Regex::new(r"^let\b").unwrap(), Some(Token::Let)),
        (Regex::new(r"^fn\b").unwrap(), Some(Token::Fn)),
        (Regex::new(r"^on\b").unwrap(), Some(Token::On)),
        (Regex::new(r"^include\b").unwrap(), Some(Token::Include)),
        (Regex::new(r"^decl\b").unwrap(), Some(Token::Decl)),

        // String literals
        (Regex::new(r#"^"([^"\\]|\\.)*""#).unwrap(), Some(Token::StringLiteral(String::new()))),

        // Numbers
        (Regex::new(r"^\d+(\.\d+)?").unwrap(), Some(Token::Number(String::new()))),

        // Identifiers (must come after keywords)
        (Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap(), Some(Token::Identifier(String::new()))),

        // Single character tokens
        (Regex::new(r"^;").unwrap(), Some(Token::Semicolon)),
        (Regex::new(r"^\|").unwrap(), Some(Token::Pipe)),
        (Regex::new(r"^:").unwrap(), Some(Token::Colon)),
        (Regex::new(r"^\{").unwrap(), Some(Token::OpenCurly)),
        (Regex::new(r"^\}").unwrap(), Some(Token::CloseCurly)),
        (Regex::new(r"^\(").unwrap(), Some(Token::OpenParen)),
        (Regex::new(r"^\)").unwrap(), Some(Token::CloseParen)),
        (Regex::new(r"^\.").unwrap(), Some(Token::Dot)),
        (Regex::new(r"^=").unwrap(), Some(Token::Equals)),
        (Regex::new(r"^_").unwrap(), Some(Token::Underscore)),
        (Regex::new(r"^\[").unwrap(), Some(Token::OpenBracket)),
        (Regex::new(r"^\]").unwrap(), Some(Token::CloseBracket)),
        (Regex::new(r"^,").unwrap(), Some(Token::Comma)),
        (Regex::new(r"^#").unwrap(), Some(Token::Hash)),
        (Regex::new(r"^@").unwrap(), Some(Token::At)),
    ];
}
