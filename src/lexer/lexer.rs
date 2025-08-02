use crate::lexer::token::{Token, TokenKind};
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

fn default_handler(kind: TokenKind, value: &'static str) -> TokenHandler {
    Box::new(move |_| Ok(Some(Token::new(kind, value.to_string()))))
}

fn skip_handler() -> TokenHandler {
    Box::new(|_| Ok(None))
}

fn literal_handler(kind: TokenKind) -> TokenHandler {
    Box::new(move |val| Ok(Some(Token::new(kind, val.to_string()))))
}

fn string_literal_handler() -> TokenHandler {
    Box::new(|val: &str| {
        let inner = &val[1..val.len() - 1];
        Ok(Some(Token::new(
            TokenKind::StringLiteral,
            inner.to_string(),
        )))
    })
}

fn identifier_handler() -> TokenHandler {
    Box::new(|val| {
        if let Some(kind) = TokenKind::lookup_reserved(val) {
            Ok(Some(Token::new(kind, val.to_string())))
        } else {
            Ok(Some(Token::new(TokenKind::Identifier, val.to_string())))
        }
    })
}

fn comment_handler() -> TokenHandler {
    Box::new(|_| Ok(None))
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
    // For special handlers like skip_handler(), comment_handler(), identifier_handler()
    ($pattern:expr, $handler:expr) => {
        RegexHandler::new(Regex::new($pattern).unwrap(), $handler)
    };

    // For literal handlers with a TokenKind
    ($pattern:expr, literal $kind:expr) => {
        RegexHandler::new(Regex::new($pattern).unwrap(), literal_handler($kind))
    };

    // For default handlers with TokenKind and literal value
    ($pattern:expr, $kind:expr, $value:expr) => {
        RegexHandler::new(
            Regex::new($pattern).unwrap(),
            default_handler($kind, $value),
        )
    };
}

use TokenKind as T;
lazy_static! {
    static ref REGEXES: Vec<RegexHandler> = vec![
        // Whitespace (check first to skip efficiently)
        regex_handler!(r"^\s+", skip_handler()),

        // Comments (must come after whitespace but before other tokens)
        regex_handler!(r"^//[^\n]*", comment_handler()),

        // Multi-character operators (must come before single chars)
        regex_handler!(r"^::", T::DoubleColon, "::"),
        regex_handler!(r"^->", T::Arrow, "->"),
        regex_handler!(r"^&&", T::And, "&&"),
        regex_handler!(r"^\|\|", T::Or, "||"),
        regex_handler!(r"^\.\.", T::DotDot, ".."),
        regex_handler!(r"^<=", T::LessEquals, "<="),
        regex_handler!(r"^>=", T::MoreEquals, ">="),
        regex_handler!(r"^==", T::EqualsEquals, "=="),
        regex_handler!(r"^!=", T::NotEquals, "!="),
        regex_handler!(r"^\+=", T::PlusEquals, "+="),
        regex_handler!(r"^-=", T::MinusEquals, "-="),
        regex_handler!(r"^\*=", T::StarEquals, "*="),
        regex_handler!(r"^/=", T::SlashEquals, "/="),
        regex_handler!(r"^%=", T::PercentEquals, "%="),

        // String literals
        regex_handler!(r#"^"[^"]*""#, string_literal_handler()),

        // Numbers
        regex_handler!(r"^[0-9]+(\.[0-9]+)?", literal T::Number),

        // Identifiers (must come after keywords)
        regex_handler!(r"^[@]?[a-zA-Z_][a-zA-Z0-9_]*", identifier_handler()),

        // Single character tokens
        regex_handler!(r"^;", T::Semicolon, ";"),
        regex_handler!(r"^&", T::Reference, "&"),
        regex_handler!(r"^\+", T::Plus, "+"),
        regex_handler!(r"^\-", T::Dash, "-"),
        regex_handler!(r"^\*", T::Star, "*"),
        regex_handler!(r"^/", T::Slash, "/"),
        regex_handler!(r"^%", T::Percent, "%"),
        regex_handler!(r"^\|", T::Pipe, "|"),
        regex_handler!(r"^:", T::Colon, ":"),
        regex_handler!(r"^\{", T::OpenCurly, "{"),
        regex_handler!(r"^\}", T::CloseCurly, "}"),
        regex_handler!(r"^\(", T::OpenParen, "("),
        regex_handler!(r"^\)", T::CloseParen, ")"),
        regex_handler!(r"^\.", T::Dot, "."),
        regex_handler!(r"^=", T::Equals, "="),
        regex_handler!(r"^_", T::Underscore, "_"),
        regex_handler!(r"^\[", T::OpenBracket, "["),
        regex_handler!(r"^\]", T::CloseBracket, "]"),
        regex_handler!(r"^,", T::Comma, ","),
        regex_handler!(r"^#", T::Hash, "#"),
        regex_handler!(r"^>", T::More, ">"),
        regex_handler!(r"^<", T::Less, "<"),
    ];
}
