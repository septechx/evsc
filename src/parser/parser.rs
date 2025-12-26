use colored::Colorize;

use crate::{
    ast::{Statement, statements::BlockStmt},
    lexer::token::{Token, TokenKind},
    parser::{lookups::create_token_lookups, stmt::parse_stmt, types::create_token_type_lookups},
};

use anyhow::{Result, bail};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
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
                location: prev_token.location,
                value: "".to_string(),
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
            bail!(
                err.unwrap_or(format!(
                    "Expected {:?} but recieved {:?} instead.",
                    expected_kind, token.kind
                ))
                .red()
                .bold()
            );
        }

        Ok(self.advance())
    }

    pub fn expect(&mut self, expected_kind: TokenKind) -> Result<Token> {
        self.expect_error(expected_kind, None)
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<BlockStmt> {
    create_token_lookups();
    create_token_type_lookups();

    let mut body: Vec<Statement> = vec![];
    let mut parser = Parser::new(tokens);

    while parser.has_tokens() {
        body.push(parse_stmt(&mut parser)?);
    }

    Ok(BlockStmt { body })
}
