pub mod token;
pub mod verify;

use std::{path::Path, sync::OnceLock};

use crate::{
    lexer::{
        token::{Token, TokenKind, TokenStream, lookup_reserved},
        verify::verify_tokens,
    },
    span::{ModuleId, Span},
};
use anyhow::Result;
use parking_lot::Once;
use regex::Regex;

type TokenHandler = Box<dyn Fn(&str, Span, ModuleId) -> Result<Option<Token>> + Send + Sync>;

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

    pub fn tokenize(&mut self, module_id: ModuleId) -> Result<TokenStream> {
        let mut tokens: Vec<Token> = vec![];

        if self.file_content.starts_with("#!") {
            self.advance(
                self.remaining_input()
                    .find('\n')
                    .unwrap_or(self.file_content.len()),
            );
        }

        while !self.at_eof() {
            let remaining = self.remaining_input();
            let mut matched = false;
            let mut match_len = 0;
            let current_pos = self.pos;

            let regexes = REGEXES.get().expect("Regexes not initialized");
            for handler in regexes.iter() {
                if let Some(mat) = handler.regex.find(remaining)
                    && mat.start() == 0
                {
                    let matched_text = mat.as_str();
                    let span = Span::new(
                        current_pos as u32,
                        (current_pos + matched_text.len()) as u32,
                    );
                    if let Some(token) = (handler.handler)(matched_text, span, module_id)? {
                        tokens.push(token);
                    }
                    match_len = matched_text.len();
                    matched = true;
                    break;
                }
            }

            if !matched {
                let next_char = remaining.chars().next().unwrap_or('\0');
                let span = Span::new(current_pos as u32, (current_pos + 1) as u32);
                tokens.push(Token {
                    kind: TokenKind::Illegal,
                    span,
                    module_id,
                    value: next_char.to_string().into(),
                });
                match_len = 1;
            }

            self.advance(match_len);
        }

        Ok(tokens.into())
    }
}

pub fn tokenize(file: String, path: &Path) -> Result<(TokenStream, ModuleId)> {
    initialize_regexes();

    let module_id = crate::SOURCE_MAPS.with(|sm| {
        let mut maps = sm.borrow_mut();
        maps.add_source(file.clone(), path.to_path_buf())
    });

    let mut lexer = Lexer::new(file);
    let tokens = lexer.tokenize(module_id)?;
    verify_tokens(&tokens);
    Ok((tokens, module_id))
}

fn default_handler(tok: TokenKind) -> TokenHandler {
    Box::new(move |value, span, module_id| {
        Ok(Some(Token {
            kind: tok,
            span,
            module_id,
            value: value.into(),
        }))
    })
}

fn number_handler() -> TokenHandler {
    Box::new(|val, span, module_id| {
        Ok(Some(Token {
            kind: TokenKind::Number,
            span,
            module_id,
            value: val.into(),
        }))
    })
}

fn skip_handler() -> TokenHandler {
    Box::new(|_, _, _| Ok(None))
}

fn string_literal_handler() -> TokenHandler {
    Box::new(|val, span, module_id| {
        let inner = &val[1..val.len() - 1];
        Ok(Some(Token {
            kind: TokenKind::StringLiteral,
            span,
            module_id,
            value: inner.into(),
        }))
    })
}

fn char_literal_handler() -> TokenHandler {
    Box::new(|val, span, module_id| {
        let inner = &val[1..val.len() - 1];
        Ok(Some(Token {
            kind: TokenKind::CharLiteral,
            span,
            module_id,
            value: inner.into(),
        }))
    })
}

fn identifier_handler() -> TokenHandler {
    Box::new(|val, span, module_id| {
        let tok = lookup_reserved(val).unwrap_or(TokenKind::Identifier);
        Ok(Some(Token {
            kind: tok,
            span,
            module_id,
            value: val.into(),
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

static INITIALIZE: Once = Once::new();
static REGEXES: OnceLock<Vec<RegexHandler>> = OnceLock::new();

fn initialize_regexes() {
    INITIALIZE.call_once(|| {
        use TokenKind as T;
        let regexes = vec![
            // Skip
            regex_handler!(r"^\s+", skip_handler()),
            regex_handler!(r"^//[^\n]*", skip_handler()),
            // Multi-char
            regex_handler!(r"^::", def T::ColonColon),
            regex_handler!(r"^->", def T::Arrow),
            regex_handler!(r"^&&", def T::And),
            regex_handler!(r"^\|\>", def T::Pipe),
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
            regex_handler!(r"<<", def T::ShiftLeft),
            regex_handler!(r">>", def T::ShiftRight),
            // Lit & Ident
            regex_handler!(r#"^"[^"]*""#, string_literal_handler()),
            regex_handler!(r"^'[^']'", char_literal_handler()),
            regex_handler!(r"^[0-9]+(\.[0-9]+)?", number_handler()),
            regex_handler!(r"^[@]?[a-zA-Z_][a-zA-Z0-9_]*", identifier_handler()),
            // Single-char
            regex_handler!(r"^;", def T::Semicolon),
            regex_handler!(r"^&", def T::Reference),
            regex_handler!(r"^\^", def T::Xor),
            regex_handler!(r"^\+", def T::Plus),
            regex_handler!(r"^\-", def T::Dash),
            regex_handler!(r"^\*", def T::Star),
            regex_handler!(r"^/", def T::Slash),
            regex_handler!(r"^%", def T::Percent),
            regex_handler!(r"^\|", def T::Bar),
            regex_handler!(r"^\?", def T::Question),
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
        let _ = REGEXES.set(regexes);
    });
}
