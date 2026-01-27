use anyhow::Result;
use thin_vec::ThinVec;

use crate::{
    ast::{Ident, Path},
    fatal_at,
    lexer::token::{Token, TokenKind},
    parser::Parser,
    span::Span,
};

pub fn unexpected_token(token: Token) -> ! {
    fatal_at!(
        token.span,
        token.module_id,
        format!("Syntax error: Unexpected token `{}`", token.value)
    )
}

pub fn parse_path(parser: &mut Parser) -> Result<Path> {
    let start = parser.current_token().span;
    let mut segments = ThinVec::new();

    let segment = parser.expect_identifier()?;
    let mut last = segment.span.end();
    segments.push(segment);

    while parser.current_token().kind == TokenKind::ColonColon
        && parser.peek().kind == TokenKind::Identifier
    {
        parser.advance();
        let segment = parser.expect_identifier()?;
        last = segment.span.end();
        segments.push(segment);
    }

    Ok(Path {
        segments,
        span: Span::new(start.start(), last),
    })
}

pub fn parse_rename(parser: &mut Parser) -> Result<Option<Ident>> {
    if parser.current_token().kind == TokenKind::As {
        parser.advance();
        Ok(Some(parser.expect_identifier()?))
    } else {
        Ok(None)
    }
}
