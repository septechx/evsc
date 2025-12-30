mod attributes;
mod expr;
mod lookups;
mod modifiers;
mod stmt;
mod string;
mod types;
mod utils;

use crate::{
    ast::{Ast, Attribute, Expr, ExprKind, NodeId, Stmt, StmtKind},
    errors::{CodeLine, CodeType, builders},
    lexer::token::{Token, TokenKind, TokenStream},
    parser::{lookups::create_token_lookups, stmt::parse_stmt, types::create_token_type_lookups},
    span::Span,
};

use anyhow::Result;

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
            let (_, line, ..) = crate::SOURCE_MAPS.with(|sm| {
                let maps = sm.borrow();
                maps.get_source(token.module_id)
                    .map(|sm| sm.span_to_source_location(&token.span))
                    .unwrap_or(Default::default())
            });

            let line_content = crate::SOURCE_MAPS.with(|sm| {
                let maps = sm.borrow();
                maps.get_source(token.module_id)
                    .and_then(|sm| sm.get_line(line))
                    .unwrap_or("")
                    .to_string()
            });

            crate::ERRORS.with(|e| {
                e.borrow_mut().add(
                    builders::fatal(err.unwrap_or(format!(
                        "Syntax error: Expected {} but recieved {} instead.",
                        expected_kind, token.kind
                    )))
                    .with_span(token.span, token.module_id)
                    .with_code(CodeLine::new(
                        line,
                        line_content,
                        CodeType::None,
                    )),
                );
            });
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
