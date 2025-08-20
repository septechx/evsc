use std::mem;

use crate::{
    ast::{ast::Statement, statements::BlockStmt},
    errors::helpers,
    lexer::token::Token,
    parser::{lookups::create_token_lookups, stmt::parse_stmt, types::create_token_type_lookups},
};

use anyhow::Result;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn current_token(&self) -> Token {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].clone()
        } else {
            Token::Eof
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

    pub fn expect_error(&mut self, expected_kind: Token, err: Option<String>) -> Result<Token> {
        let token = self.current_token();

        if mem::discriminant(&token) != mem::discriminant(&expected_kind) {
            let error_message = err
                .unwrap_or(format!(
                    "Expected {expected_kind:?} but recieved {token:?} instead.",
                ));
            
            helpers::add_error(error_message);
            
            return Ok(self.advance());
        }

        Ok(self.advance())
    }

    pub fn expect(&mut self, expected_kind: Token) -> Result<Token> {
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
