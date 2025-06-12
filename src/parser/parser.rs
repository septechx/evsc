use colored::Colorize;

use crate::{
    ast::{ast::Stmt, statements::BlockStmt},
    lexer::token::{Token, TokenKind},
};

use super::{lookups::create_token_lookups, stmt::parse_stmt};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        create_token_lookups();
        Self { tokens, pos: 0 }
    }

    pub fn current_token(&self) -> Token {
        self.tokens[self.pos].clone()
    }

    pub fn current_token_kind(&self) -> TokenKind {
        self.current_token().kind
    }

    pub fn advance(&mut self) -> Token {
        let token = self.current_token();
        self.pos += 1;
        token
    }

    pub fn has_tokens(&self) -> bool {
        self.pos < self.tokens.len() && self.current_token_kind() != TokenKind::Eof
    }

    pub fn expect_error(&mut self, expected_kind: TokenKind, err: Option<String>) -> Token {
        let token = self.current_token();
        let kind = token.kind;

        if kind != expected_kind {
            let err = err.unwrap_or(format!(
                "Expected {:?} but recieved {:?} instead.",
                expected_kind, kind
            ));

            panic!("{}", err.red().bold())
        }

        self.advance()
    }

    pub fn expect(&mut self, expected_kind: TokenKind) -> Token {
        self.expect_error(expected_kind, None)
    }
}

pub fn parse(tokens: Vec<Token>) -> BlockStmt {
    let mut body: Vec<Box<dyn Stmt>> = vec![];
    let mut parser = Parser::new(tokens);

    while parser.has_tokens() {
        body.push(parse_stmt(&mut parser));
    }

    BlockStmt { body }
}
