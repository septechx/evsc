pub mod token;
pub mod verify;

use std::path::Path;

use crate::{
    errors::SourceLocation,
    lexer::{
        token::{Token, TokenKind, TokenStream},
        verify::verify_tokens,
    },
};
use anyhow::Result;
use lazy_static::lazy_static;
use regex::Regex;

type TokenHandler = Box<dyn Fn(&str, SourceLocation) -> Result<Option<Token>> + Send + Sync>;

#[derive(Debug, Clone)]
pub struct Lexer {
    file_content: String,
    file_len: usize,
    pos: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(file: String) -> Self {
        Self {
            file_len: file.len(),
            file_content: file,
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    fn at_eof(&self) -> bool {
        self.pos >= self.file_len
    }

    fn advance(&mut self, len: usize) {
        let advanced_text = &self.file_content[self.pos..self.pos + len];
        let newlines = advanced_text.matches('\n').count();
        if newlines > 0 {
            self.line += newlines;
            if let Some(last_newline) = advanced_text.rfind('\n') {
                self.column = advanced_text[last_newline + 1..].len() + 1;
            }
        } else {
            self.column += len;
        }
        self.pos += len;
    }

    fn remaining_input(&self) -> &str {
        &self.file_content[self.pos..]
    }

    pub fn tokenize(&mut self, file_path: &Path) -> Result<TokenStream> {
        let mut tokens: Vec<Token> = vec![];

        while !self.at_eof() {
            let remaining = self.remaining_input();
            let mut matched = false;
            let mut match_len = 0;
            let current_line = self.line;
            let current_column = self.column;

            for handler in REGEXES.iter() {
                if let Some(mat) = handler.regex.find(remaining)
                    && mat.start() == 0
                {
                    let matched_text = mat.as_str();
                    if let Some(token) = (handler.handler)(
                        matched_text,
                        SourceLocation::new(
                            file_path.to_path_buf(),
                            current_line,
                            current_column,
                            match_len,
                        ),
                    )? {
                        tokens.push(token);
                    }
                    match_len = matched_text.len();
                    matched = true;
                    break;
                }
            }

            if !matched {
                let next_char = remaining.chars().next().unwrap_or('\0');
                tokens.push(Token {
                    kind: TokenKind::Illegal,
                    location: SourceLocation::new(
                        file_path.to_path_buf(),
                        current_line,
                        current_column,
                        1,
                    ),
                    value: next_char.to_string(),
                });
                match_len = 1;
            }

            self.advance(match_len);
        }

        Ok(TokenStream(tokens))
    }
}

pub fn tokenize(file: String, path: &Path) -> Result<TokenStream> {
    let mut lexer = Lexer::new(file);
    let tokens = lexer.tokenize(path)?;
    verify_tokens(&tokens);
    Ok(tokens)
}

fn default_handler(tok: TokenKind) -> TokenHandler {
    Box::new(move |value, location| {
        Ok(Some(Token {
            location,
            kind: tok,
            value: value.to_string(),
        }))
    })
}

fn number_handler() -> TokenHandler {
    Box::new(|val, location| {
        Ok(Some(Token {
            location,
            kind: TokenKind::Number,
            value: val.to_string(),
        }))
    })
}

fn skip_handler() -> TokenHandler {
    Box::new(|_, _| Ok(None))
}

fn string_literal_handler() -> TokenHandler {
    Box::new(|val, location| {
        let inner = &val[1..val.len() - 1];
        Ok(Some(Token {
            location,
            kind: TokenKind::StringLiteral,
            value: inner.to_string(),
        }))
    })
}

fn identifier_handler() -> TokenHandler {
    Box::new(|val, location| {
        let tok = Token::lookup_reserved(val).unwrap_or(TokenKind::Identifier);
        Ok(Some(Token {
            location,
            kind: tok,
            value: val.to_string(),
        }))
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

use TokenKind as T;
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
        regex_handler!(r"^\$", def T::Dollar),
    ];
}
