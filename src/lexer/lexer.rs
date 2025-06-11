use crate::lexer::token::Token;
use colored::Colorize;
use lazy_static::lazy_static;
use regex::Regex;
use std::sync::Arc;

type TokenHandler = Arc<dyn Fn(&str) -> Option<Token> + Send + Sync>;

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

            for handler in REGEXES.iter() {
                if let Some(mat) = handler.regex.find(remaining) {
                    if mat.start() == 0 {
                        let matched_text = mat.as_str();
                        if let Some(token) = (handler.handler)(matched_text) {
                            tokens.push(token);
                        }
                        self.advance(matched_text.len());
                        matched = true;
                        break;
                    }
                }
            }

            if !matched {
                tokens.push(Token::Unknown(remaining.chars().next().unwrap_or('\0')));
                self.advance(1);

                eprintln!(
                    "{}",
                    format!(
                        "[Lexer/ERROR] Unexpected character at position {}: '{}'",
                        self.pos,
                        remaining.chars().next().unwrap_or('\0'),
                    )
                    .red()
                    .bold()
                );
            }
        }

        tokens
    }
}

fn default_handler(token: Token) -> TokenHandler {
    Arc::new(move |_| Some(token.clone()))
}

fn skip_handler() -> TokenHandler {
    Arc::new(|_| None)
}

fn token_with_content_handler(token_type: fn(String) -> Token) -> TokenHandler {
    Arc::new(move |content| {
        if token_type as usize == Token::StringLiteral as usize {
            let value = content[1..content.len() - 1].to_string();
            Some(Token::StringLiteral(value))
        } else if token_type as usize == Token::Identifier as usize {
            Some(Token::Identifier(content.to_string()))
        } else if token_type as usize == Token::Number as usize {
            Some(Token::Number(content.to_string()))
        } else {
            None
        }
    })
}

struct RegexHandler {
    regex: Regex,
    handler: TokenHandler,
}

impl RegexHandler {
    fn new(regex: Regex, handler: TokenHandler) -> Self {
        Self { regex, handler }
    }
}

lazy_static! {
    static ref REGEXES: Vec<RegexHandler> = vec![
        // Whitespace (check first to skip efficiently)
        RegexHandler::new(Regex::new(r"^\s+").unwrap(), skip_handler()),

        // Multi-character operators (must come before single chars)
        RegexHandler::new(Regex::new(r"^::").unwrap(), default_handler(Token::DoubleColon)),
        RegexHandler::new(Regex::new(r"^:=").unwrap(), default_handler(Token::ColonEquals)),
        RegexHandler::new(Regex::new(r"^->").unwrap(), default_handler(Token::Arrow)),

        // Keywords (must come before general identifier)
        RegexHandler::new(Regex::new(r"^let\b").unwrap(), default_handler(Token::Let)),
        RegexHandler::new(Regex::new(r"^fn\b").unwrap(), default_handler(Token::Fn)),
        RegexHandler::new(Regex::new(r"^on\b").unwrap(), default_handler(Token::On)),
        RegexHandler::new(Regex::new(r"^include\b").unwrap(), default_handler(Token::Include)),
        RegexHandler::new(Regex::new(r"^decl\b").unwrap(), default_handler(Token::Decl)),

        // String literals
        RegexHandler::new(Regex::new(r#"^"([^"\\]|\\.)*""#).unwrap(), token_with_content_handler(Token::StringLiteral)),

        // Numbers
        RegexHandler::new(Regex::new(r"^\d+(\.\d+)?").unwrap(), token_with_content_handler(Token::Number)),

        // Identifiers (must come after keywords)
        RegexHandler::new(Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap(), token_with_content_handler(Token::Identifier)),

        // Single character tokens
        RegexHandler::new(Regex::new(r"^;").unwrap(), default_handler(Token::Semicolon)),
        RegexHandler::new(Regex::new(r"^\|").unwrap(), default_handler(Token::Pipe)),
        RegexHandler::new(Regex::new(r"^:").unwrap(), default_handler(Token::Colon)),
        RegexHandler::new(Regex::new(r"^\{").unwrap(), default_handler(Token::OpenCurly)),
        RegexHandler::new(Regex::new(r"^\}").unwrap(), default_handler(Token::CloseCurly)),
        RegexHandler::new(Regex::new(r"^\(").unwrap(), default_handler(Token::OpenParen)),
        RegexHandler::new(Regex::new(r"^\)").unwrap(), default_handler(Token::CloseParen)),
        RegexHandler::new(Regex::new(r"^\.").unwrap(), default_handler(Token::Dot)),
        RegexHandler::new(Regex::new(r"^=").unwrap(), default_handler(Token::Equals)),
        RegexHandler::new(Regex::new(r"^_").unwrap(), default_handler(Token::Underscore)),
        RegexHandler::new(Regex::new(r"^\[").unwrap(), default_handler(Token::OpenBracket)),
        RegexHandler::new(Regex::new(r"^\]").unwrap(), default_handler(Token::CloseBracket)),
        RegexHandler::new(Regex::new(r"^,").unwrap(), default_handler(Token::Comma)),
        RegexHandler::new(Regex::new(r"^#").unwrap(), default_handler(Token::Hash)),
        RegexHandler::new(Regex::new(r"^@").unwrap(), default_handler(Token::At)),
    ];
}
