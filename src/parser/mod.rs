mod attributes;
mod expr;
mod lookups;
mod modifiers;
mod stmt;
mod string;
mod types;
mod utils;

use crate::{
    ast::{Ast, Ident, Stmt},
    fatal_at,
    lexer::token::{Token, TokenKind, TokenStream},
    parser::{lookups::create_token_lookups, stmt::parse_stmt, types::create_token_type_lookups},
};

use anyhow::Result;
use std::convert::TryInto;
use thin_vec::ThinVec;

pub struct Parser {
    tokens: ThinVec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: TokenStream) -> Self {
        Parser {
            tokens: tokens.0,
            pos: 0,
        }
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn current_token(&self) -> Token {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].clone()
        } else {
            let prev_token = self.tokens[self.tokens.len() - 1].clone();
            Token {
                kind: TokenKind::Eof,
                span: prev_token.span,
                module_id: prev_token.module_id,
                value: "".into(),
            }
        }
    }

    pub fn peek(&self) -> Token {
        if self.pos + 1 < self.tokens.len() {
            self.tokens[self.pos + 1].clone()
        } else {
            let prev_token = self.tokens[self.tokens.len() - 1].clone();
            Token {
                kind: TokenKind::Eof,
                span: prev_token.span,
                module_id: prev_token.module_id,
                value: "".into(),
            }
        }
    }

    pub fn advance(&mut self) -> Token {
        let current = self.current_token();
        self.pos += 1;
        current
    }

    pub fn has_tokens(&self) -> bool {
        self.pos < self.tokens.len()
    }

    pub fn expect_error(&mut self, expected_kind: TokenKind, err: Option<String>) -> Result<Token> {
        let token = self.current_token();

        if token.kind != expected_kind {
            fatal_at!(
                token.span,
                token.module_id,
                err.unwrap_or_else(|| format!(
                    "Syntax error: Expected {} but received {} instead.",
                    expected_kind, token.kind
                ))
            );
        }

        Ok(self.advance())
    }

    pub fn expect(&mut self, expected_kind: TokenKind) -> Result<Token> {
        self.expect_error(expected_kind, None)
    }

    pub fn expect_identifier(&mut self) -> Result<Ident> {
        let token = self.expect(TokenKind::Identifier)?;
        TryInto::<Ident>::try_into(token)
    }
}

pub fn parse(tokens: TokenStream, name: &str) -> Result<Ast> {
    create_token_lookups();
    create_token_type_lookups();

    let mut body: ThinVec<Stmt> = ThinVec::new();
    let mut parser = Parser::new(tokens);

    while parser.has_tokens() {
        body.push(parse_stmt(&mut parser)?);
    }

    Ok(Ast {
        name: name.into(),
        items: body,
    })
}
