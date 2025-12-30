mod attributes;
mod expr;
mod lookups;
mod stmt;
mod string;
pub mod types;

use colored::Colorize;

use crate::{
    ast::{Ast, Expr, ExprKind, NodeId, Stmt, StmtKind},
    lexer::token::{Token, TokenKind, TokenStream},
    parser::{lookups::create_token_lookups, stmt::parse_stmt, types::create_token_type_lookups},
};

use anyhow::{Result, bail};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    cur_node_id: usize,
}

impl Parser {
    pub fn new(tokens: TokenStream) -> Self {
        Parser {
            tokens: tokens.0,
            pos: 0,
            cur_node_id: 0,
        }
    }

    pub fn stmt(&mut self, kind: StmtKind) -> Stmt {
        Stmt {
            id: self.next_id(),
            kind,
        }
    }

    pub fn expr(&mut self, kind: ExprKind) -> Expr {
        Expr {
            id: self.next_id(),
            kind,
        }
    }

    pub fn next_id(&mut self) -> NodeId {
        self.cur_node_id += 1;
        NodeId(self.cur_node_id)
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

pub fn parse(tokens: TokenStream) -> Result<Ast> {
    create_token_lookups();
    create_token_type_lookups();

    let mut body: Vec<Stmt> = vec![];
    let mut parser = Parser::new(tokens);

    while parser.has_tokens() {
        body.push(parse_stmt(&mut parser)?);
    }

    Ok(Ast(body))
}
