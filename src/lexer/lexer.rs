use std::path::Path;

use crate::lexer::{token::{Token, LocatedToken}, verify::verify_tokens};
use anyhow::Result;
use lazy_static::lazy_static;
use regex::Regex;

type TokenHandler = Box<dyn Fn(&str) -> Result<Option<Token>> + Send + Sync>;

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

    pub fn tokenize(&mut self, file_path: &Path) -> Result<Vec<LocatedToken>> {
        let mut tokens: Vec<LocatedToken> = vec!();

        while !self.at_eof() {
            let remaining = self.remaining_input();
            let mut matched = false;
            let mut match_len = 0;
            let current_line = self.line;
            let current_column = self.column;

            for handler in REGEXES.iter() {
                if let Some(mat) = handler.regex.find(remaining) {
                    if mat.start() == 0 {
                        let matched_text = mat.as_str();
                        if let Some(token) = (handler.handler)(matched_text)? {
                            tokens.push(LocatedToken::new_simple(
                                token,
                                file_path.to_path_buf(),
                                current_line,
                                current_column,
                            ));
                        }
                        match_len = matched_text.len();
                        matched = true;
                        break;
                    }
                }
            }

            if !matched {
                let next_char = remaining.chars().next().unwrap_or('\0');
                tokens.push(LocatedToken::new_simple(
                    Token::Illegal(next_char),
                    file_path.to_path_buf(),
                    current_line,
                    current_column,
                ));
                match_len = 1;
            }

            self.advance(match_len);
        }

        Ok(tokens)
    }
}

pub fn tokenize(file: String, path: &Path) -> Result<Vec<LocatedToken>> {
    let mut lexer = Lexer::new(file);
    let tokens = lexer.tokenize(path)?;
    verify_tokens(&tokens);
    Ok(tokens)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let input = r#"
        let five = 5;
        let ten = 10;
        let add = (a, b) -> a + b;
        let result = add(five, ten);
        "#;
        let test_path = Path::new("test.evsc");
        let expected = vec![
            LocatedToken::new_simple(Token::Let, test_path.to_path_buf(), 1, 1),
            LocatedToken::new_simple(Token::Identifier("five".to_string()), test_path.to_path_buf(), 1, 5),
            LocatedToken::new_simple(Token::Equals, test_path.to_path_buf(), 1, 9),
            LocatedToken::new_simple(Token::Number(5), test_path.to_path_buf(), 1, 11),
            LocatedToken::new_simple(Token::Semicolon, test_path.to_path_buf(), 1, 12),
            LocatedToken::new_simple(Token::Let, test_path.to_path_buf(), 2, 1),
            LocatedToken::new_simple(Token::Identifier("ten".to_string()), test_path.to_path_buf(), 2, 5),
            LocatedToken::new_simple(Token::Equals, test_path.to_path_buf(), 2, 9),
            LocatedToken::new_simple(Token::Number(10), test_path.to_path_buf(), 2, 11),
            LocatedToken::new_simple(Token::Semicolon, test_path.to_path_buf(), 2, 12),
            LocatedToken::new_simple(Token::Let, test_path.to_path_buf(), 3, 1),
            LocatedToken::new_simple(Token::Identifier("add".to_string()), test_path.to_path_buf(), 3, 5),
            LocatedToken::new_simple(Token::Equals, test_path.to_path_buf(), 3, 9),
            LocatedToken::new_simple(Token::OpenParen, test_path.to_path_buf(), 3, 11),
            LocatedToken::new_simple(Token::Identifier("a".to_string()), test_path.to_path_buf(), 3, 13),
            LocatedToken::new_simple(Token::Comma, test_path.to_path_buf(), 3, 14),
            LocatedToken::new_simple(Token::Identifier("b".to_string()), test_path.to_path_buf(), 3, 16),
            LocatedToken::new_simple(Token::CloseParen, test_path.to_path_buf(), 3, 17),
            LocatedToken::new_simple(Token::Arrow, test_path.to_path_buf(), 3, 19),
            LocatedToken::new_simple(Token::Identifier("a".to_string()), test_path.to_path_buf(), 3, 21),
            LocatedToken::new_simple(Token::Plus, test_path.to_path_buf(), 3, 23),
            LocatedToken::new_simple(Token::Identifier("b".to_string()), test_path.to_path_buf(), 3, 25),
            LocatedToken::new_simple(Token::Semicolon, test_path.to_path_buf(), 3, 26),
            LocatedToken::new_simple(Token::Let, test_path.to_path_buf(), 4, 1),
            LocatedToken::new_simple(Token::Identifier("result".to_string()), test_path.to_path_buf(), 4, 5),
            LocatedToken::new_simple(Token::Equals, test_path.to_path_buf(), 4, 13),
            LocatedToken::new_simple(Token::Identifier("add".to_string()), test_path.to_path_buf(), 4, 15),
            LocatedToken::new_simple(Token::OpenParen, test_path.to_path_buf(), 4, 17),
            LocatedToken::new_simple(Token::Identifier("five".to_string()), test_path.to_path_buf(), 4, 19),
            LocatedToken::new_simple(Token::Comma, test_path.to_path_buf(), 4, 21),
            LocatedToken::new_simple(Token::Identifier("ten".to_string()), test_path.to_path_buf(), 4, 23),
            LocatedToken::new_simple(Token::CloseParen, test_path.to_path_buf(), 4, 24),
            LocatedToken::new_simple(Token::Semicolon, test_path.to_path_buf(), 4, 25),
        ];
        assert_eq!(
            tokenize(input.to_string(), &Path::new("test.evsc")).unwrap().into_iter().map(|lt| lt.token).collect::<Vec<_>>(),
            expected.into_iter().map(|lt| lt.token).collect::<Vec<_>>()
        );
    }
}
