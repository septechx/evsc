mod attributes;
mod expr;
mod lookups;
mod modifiers;
mod stmt;
mod string;
mod types;
mod utils;

use crate::{
    ast::{Ast, Attribute, Expr, ExprKind, Ident, NodeId, Stmt, StmtKind, Type, TypeKind},
    errors::{
        builders,
        widgets::{CodeWidget, LocationWidget},
    },
    lexer::token::{Token, TokenKind, TokenStream},
    parser::{lookups::create_token_lookups, stmt::parse_stmt, types::create_token_type_lookups},
    span::Span,
};

use anyhow::Result;
use std::convert::TryInto;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    next_id: NodeId,
}

impl Parser {
    pub fn new(tokens: TokenStream) -> Self {
        Parser {
            tokens: tokens.0,
            pos: 0,
            next_id: NodeId(0),
        }
    }

    pub fn stmt(&mut self, kind: StmtKind, span: Span, attributes: Vec<Attribute>) -> Stmt {
        Stmt {
            id: self.next_id(),
            kind,
            span,
            attributes,
        }
    }

    pub fn expr(&mut self, kind: ExprKind, span: Span) -> Expr {
        Expr {
            id: self.next_id(),
            kind,
            span,
        }
    }

    pub fn type_(&mut self, kind: TypeKind, span: Span) -> Type {
        Type {
            id: self.next_id(),
            kind,
            span,
        }
    }

    pub fn next_id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id = NodeId(self.next_id.0 + 1);
        id
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
            crate::ERRORS.with(|e| -> Result<()> {
                e.borrow_mut().add(
                    builders::fatal(err.unwrap_or(format!(
                        "Syntax error: Expected {} but recieved {} instead.",
                        expected_kind, token.kind
                    )))
                    .add_widget(LocationWidget::new(token.span, token.module_id)?)
                    .add_widget(CodeWidget::new(token.span, token.module_id)?),
                );
                Ok(())
            })?;
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
