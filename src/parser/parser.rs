use std::{mem, path::PathBuf};

use crate::{
    ast::{ast::Statement, statements::BlockStmt},
    errors::ErrorLevel,
    lexer::token::{LocatedToken, Token},
    parser::{lookups::create_token_lookups, stmt::parse_stmt, types::create_token_type_lookups},
    ERRORS,
};

use anyhow::Result;

pub struct Parser {
    tokens: Vec<LocatedToken>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<LocatedToken>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn current_token(&self) -> LocatedToken {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].clone()
        } else {
            LocatedToken::new(
                Token::Eof,
                self.tokens[self.tokens.len() - 1].location.clone(),
            )
        }
    }

    pub fn advance(&mut self) -> LocatedToken {
        let current = self.current_token();
        self.pos += 1;
        current
    }

    pub fn has_tokens(&self) -> bool {
        self.pos < self.tokens.len()
    }

    pub fn expect_error(
        &mut self,
        expected_kind: Token,
        err: Option<String>,
    ) -> Result<LocatedToken> {
        let token = self.current_token();

        if mem::discriminant(&token.token) != mem::discriminant(&expected_kind) {
            let error_message = err.unwrap_or(format!(
                "Expected {expected_kind:?} but recieved {token:?} instead.",
            ));

            ERRORS
                .lock()
                .add_with_location(ErrorLevel::Error, error_message, token.location);
            return Ok(self.advance().clone());
        }

        Ok(self.advance().clone())
    }

    pub fn expect(&mut self, expected_kind: Token) -> Result<LocatedToken> {
        self.expect_error(expected_kind, None)
    }
}

pub fn parse(tokens: Vec<LocatedToken>) -> Result<BlockStmt> {
    create_token_lookups();
    create_token_type_lookups();

    let mut body: Vec<Statement> = vec![];
    let mut parser = Parser::new(tokens);

    while parser.has_tokens() {
        body.push(parse_stmt(&mut parser)?);
    }

    Ok(BlockStmt { body })
}

pub fn parse_with_file_path(tokens: Vec<LocatedToken>) -> Result<BlockStmt> {
    create_token_lookups();
    create_token_type_lookups();

    let mut body: Vec<Statement> = vec![];
    let mut parser = Parser::new(tokens);

    while parser.has_tokens() {
        body.push(parse_stmt(&mut parser)?);
    }

    Ok(BlockStmt { body })
}
