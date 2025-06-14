use crate::lexer::token::{Token, TokenKind};
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
    line: usize,
}

impl Lexer {
    pub fn new(file: String) -> Self {
        Self {
            file_len: file.len(),
            file_content: file,
            pos: 0,
            line: 1,
        }
    }

    fn at_eof(&self) -> bool {
        self.pos >= self.file_len
    }

    fn advance(&mut self, len: usize) {
        let advanced_text = &self.file_content[self.pos..self.pos + len];
        self.line += advanced_text.matches('\n').count();
        self.pos += len;
    }

    fn remaining_input(&self) -> &str {
        &self.file_content[self.pos..]
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        while !self.at_eof() {
            let remaining = self.remaining_input();
            let mut matched = false;
            let mut match_len = 0;

            for handler in REGEXES.iter() {
                if let Some(mat) = handler.regex.find(remaining) {
                    if mat.start() == 0 {
                        let matched_text = mat.as_str();
                        if let Some(token) = (handler.handler)(matched_text) {
                            tokens.push(token);
                        }
                        match_len = matched_text.len();
                        matched = true;
                        break;
                    }
                }
            }

            if !matched {
                let next_char = remaining.chars().next().unwrap_or('\0');

                panic!(
                    "{}",
                    format!(
                        "Unexpected character at line {}, position {}: '{}'",
                        self.line, self.pos, next_char,
                    )
                    .red()
                    .bold()
                );
            }

            self.advance(match_len);
        }

        tokens
    }
}

pub fn tokenize(file: String) -> Vec<Token> {
    let mut lexer = Lexer::new(file);
    lexer.tokenize()
}

fn default_handler(kind: TokenKind, value: &'static str) -> TokenHandler {
    Arc::new(move |_| Some(Token::new(kind, value.to_string())))
}

fn skip_handler() -> TokenHandler {
    Arc::new(|_| None)
}

fn literal_handler(kind: TokenKind) -> TokenHandler {
    Arc::new(move |val| Some(Token::new(kind, val.to_string())))
}

fn identifier_handler() -> TokenHandler {
    Arc::new(|val| {
        if let Some(kind) = TokenKind::lookup_reserved(val) {
            Some(Token::new(kind, val.to_string()))
        } else {
            Some(Token::new(TokenKind::Identifier, val.to_string()))
        }
    })
}

fn comment_handler() -> TokenHandler {
    Arc::new(|_| None)
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

        // Comments (must come after whitespace but before other tokens)
        RegexHandler::new(Regex::new(r"^//[^\n]*").unwrap(), comment_handler()),

        // Multi-character operators (must come before single chars)
        RegexHandler::new(Regex::new(r"^::").unwrap(), default_handler(TokenKind::DoubleColon, "::")),
        RegexHandler::new(Regex::new(r"^->").unwrap(), default_handler(TokenKind::Arrow, "->")),
        RegexHandler::new(Regex::new(r"^&&").unwrap(), default_handler(TokenKind::And, "&&")),
        RegexHandler::new(Regex::new(r"^\|\|").unwrap(), default_handler(TokenKind::Or, "||")),
        RegexHandler::new(Regex::new(r"^\.\.").unwrap(), default_handler(TokenKind::DotDot, "..")),
        RegexHandler::new(Regex::new(r"^<=").unwrap(), default_handler(TokenKind::LessEquals, "<=")),
        RegexHandler::new(Regex::new(r"^>=").unwrap(), default_handler(TokenKind::MoreEquals, ">=")),
        RegexHandler::new(Regex::new(r"^==").unwrap(), default_handler(TokenKind::EqualsEquals, "==")),
        RegexHandler::new(Regex::new(r"^!=").unwrap(), default_handler(TokenKind::NotEquals, "!=")),
        RegexHandler::new(Regex::new(r"^\+=").unwrap(), default_handler(TokenKind::PlusEquals, "+=")),
        RegexHandler::new(Regex::new(r"^-=").unwrap(), default_handler(TokenKind::MinusEquals, "-=")),

        // String literals
        RegexHandler::new(Regex::new(r#"^"[^"]*""#).unwrap(), literal_handler(TokenKind::StringLiteral)),

        // Numbers
        RegexHandler::new(Regex::new(r"^[0-9]+(\.[0-9]+)?").unwrap(), literal_handler(TokenKind::Number)),

        // Identifiers (must come after keywords)
        RegexHandler::new(Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap(), identifier_handler()),

        // Single character tokens
        RegexHandler::new(Regex::new(r"^;").unwrap(), default_handler(TokenKind::Semicolon, ";")),
        RegexHandler::new(Regex::new(r"^\+").unwrap(), default_handler(TokenKind::Plus, "+")),
        RegexHandler::new(Regex::new(r"^\-").unwrap(), default_handler(TokenKind::Dash, "-")),
        RegexHandler::new(Regex::new(r"^\*").unwrap(), default_handler(TokenKind::Star, "*")),
        RegexHandler::new(Regex::new(r"^/").unwrap(), default_handler(TokenKind::Slash, "/")),
        RegexHandler::new(Regex::new(r"^%").unwrap(), default_handler(TokenKind::Percent, "%")),
        RegexHandler::new(Regex::new(r"^\|").unwrap(), default_handler(TokenKind::Pipe, "|")),
        RegexHandler::new(Regex::new(r"^:").unwrap(), default_handler(TokenKind::Colon, ":")),
        RegexHandler::new(Regex::new(r"^\{").unwrap(), default_handler(TokenKind::OpenCurly, "{")),
        RegexHandler::new(Regex::new(r"^\}").unwrap(), default_handler(TokenKind::CloseCurly, "}")),
        RegexHandler::new(Regex::new(r"^\(").unwrap(), default_handler(TokenKind::OpenParen, "(")),
        RegexHandler::new(Regex::new(r"^\)").unwrap(), default_handler(TokenKind::CloseParen, ")")),
        RegexHandler::new(Regex::new(r"^\.").unwrap(), default_handler(TokenKind::Dot, ".")),
        RegexHandler::new(Regex::new(r"^=").unwrap(), default_handler(TokenKind::Equals, "=")),
        RegexHandler::new(Regex::new(r"^_").unwrap(), default_handler(TokenKind::Underscore, "_")),
        RegexHandler::new(Regex::new(r"^\[").unwrap(), default_handler(TokenKind::OpenBracket, "[")),
        RegexHandler::new(Regex::new(r"^\]").unwrap(), default_handler(TokenKind::CloseBracket, "]")),
        RegexHandler::new(Regex::new(r"^,").unwrap(), default_handler(TokenKind::Comma, ",")),
        RegexHandler::new(Regex::new(r"^#").unwrap(), default_handler(TokenKind::Hash, "#")),
        RegexHandler::new(Regex::new(r"^@").unwrap(), default_handler(TokenKind::At, "@")),
        RegexHandler::new(Regex::new(r"^>").unwrap(), default_handler(TokenKind::More, ">")),
        RegexHandler::new(Regex::new(r"^<").unwrap(), default_handler(TokenKind::Less, "<")),
    ];
}
