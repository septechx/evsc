use anyhow::Result;

use crate::{
    ast::{Ident, Path},
    errors::{
        builders,
        widgets::{CodeWidget, LocationWidget},
    },
    lexer::token::{Token, TokenKind},
    parser::Parser,
    span::Span,
};

pub fn unexpected_token(token: Token) -> ! {
    let span = token.span;
    let module_id = token.module_id;

    crate::ERRORS
        .with(|e| -> Result<()> {
            e.borrow_mut().add(
                builders::fatal(format!("Syntax error: Unexpected token `{}`", token.value))
                    .add_widget(LocationWidget::new(span, module_id)?)
                    .add_widget(CodeWidget::new(span, module_id)?),
            );
            Ok(())
        })
        .expect("failed to create error");

    unreachable!()
}

pub fn parse_path(parser: &mut Parser) -> Result<Path> {
    let start = parser.current_token().span;
    let mut segments = vec![];

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
        segments: segments.into_boxed_slice(),
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
