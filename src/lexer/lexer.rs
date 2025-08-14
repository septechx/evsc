use crate::lexer::token::Token;
use anyhow::{anyhow, Result};
use colored::Colorize;
use lazy_static::lazy_static;
use regex::Regex;

type TokenHandler = Box<dyn Fn(&str) -> Result<Option<Token>> + Send + Sync>;

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

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = vec![];

        while !self.at_eof() {
            let remaining = self.remaining_input();
            let mut matched = false;
            let mut match_len = 0;

            for handler in REGEXES.iter() {
                if let Some(mat) = handler.regex.find(remaining) {
                    if mat.start() == 0 {
                        let matched_text = mat.as_str();
                        if let Some(token) = (handler.handler)(matched_text)? {
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
                return Err(anyhow!(
                    "{}",
                    format!(
                        "Unexpected character at line {}, position {}: '{}'",
                        self.line, self.pos, next_char,
                    )
                    .red()
                    .bold()
                ));
            }

            self.advance(match_len);
        }

        Ok(tokens)
    }
}

pub fn tokenize(file: String) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(file);
    lexer.tokenize()
}

fn default_handler(tok: Token) -> TokenHandler {
    Box::new(move |_| Ok(Some(tok.clone())))
}

fn number_handler() -> TokenHandler {
    Box::new(move |val| Ok(Some(Token::Number(val.parse::<i32>()?))))
}

fn skip_handler() -> TokenHandler {
    Box::new(|_| Ok(None))
}

fn string_literal_handler() -> TokenHandler {
    Box::new(|val: &str| {
        let inner = &val[1..val.len() - 1];
        Ok(Some(Token::StringLiteral(inner.to_string())))
    })
}

fn identifier_handler() -> TokenHandler {
    Box::new(|val| {
        if let Some(tok) = Token::lookup_reserved(val) {
            Ok(Some(tok))
        } else {
            Ok(Some(Token::Identifier(val.to_string())))
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

macro_rules! regex_handler {
    ($pattern:expr, $handler:expr) => {
        RegexHandler::new(Regex::new($pattern).unwrap(), $handler)
    };

    ($pattern:expr, def $kind:expr) => {
        RegexHandler::new(Regex::new($pattern).unwrap(), default_handler($kind))
    };
}

use Token as T;
lazy_static! {
    static ref REGEXES: Vec<RegexHandler> = vec![
        // Skip
        regex_handler!(r"^\s+", skip_handler()),
        regex_handler!(r"^//[^\n]*", skip_handler()),
        // Multi-char
        regex_handler!(r"^->", def T::Arrow),
        regex_handler!(r"^&&", def T::And),
        regex_handler!(r"^\|\|", def T::Or),
        regex_handler!(r"^\.\.", def T::DotDot),
        regex_handler!(r"^<=", def T::LessEquals),
        regex_handler!(r"^>=", def T::MoreEquals),
        regex_handler!(r"^==", def T::EqualsEquals),
        regex_handler!(r"^!=", def T::NotEquals),
        regex_handler!(r"^\+=", def T::PlusEquals),
        regex_handler!(r"^-=", def T::MinusEquals),
        regex_handler!(r"^\*=", def T::StarEquals),
        regex_handler!(r"^/=", def T::SlashEquals),
        regex_handler!(r"^%=", def T::PercentEquals),
        // Lit & Ident
        regex_handler!(r#"^"[^"]*""#, string_literal_handler()),
        regex_handler!(r"^[0-9]+(\.[0-9]+)?", number_handler()),
        regex_handler!(r"^[@]?[a-zA-Z_][a-zA-Z0-9_]*", identifier_handler()),
        // Single-char
        regex_handler!(r"^;", def T::Semicolon),
        regex_handler!(r"^&", def T::Reference),
        regex_handler!(r"^\+", def T::Plus),
        regex_handler!(r"^\-", def T::Dash),
        regex_handler!(r"^\*", def T::Star),
        regex_handler!(r"^/", def T::Slash),
        regex_handler!(r"^%", def T::Percent),
        regex_handler!(r"^\|", def T::Pipe),
        regex_handler!(r"^:", def T::Colon),
        regex_handler!(r"^\{", def T::OpenCurly),
        regex_handler!(r"^\}", def T::CloseCurly),
        regex_handler!(r"^\(", def T::OpenParen),
        regex_handler!(r"^\)", def T::CloseParen),
        regex_handler!(r"^\.", def T::Dot),
        regex_handler!(r"^=", def T::Equals),
        regex_handler!(r"^_", def T::Underscore),
        regex_handler!(r"^\[", def T::OpenBracket),
        regex_handler!(r"^\]", def T::CloseBracket),
        regex_handler!(r"^,", def T::Comma),
        regex_handler!(r"^#", def T::Hash),
        regex_handler!(r"^>", def T::More),
        regex_handler!(r"^<", def T::Less),
    ];
}
