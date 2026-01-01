use anyhow::Result;

use crate::{
    ast::{Ident, Path, expressions::SymbolExpr},
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

    let segment = parser.expect(TokenKind::Identifier)?;
    segments.push(Ident {
        value: SymbolExpr {
            value: segment.value,
        },
        span: segment.span,
    });

    let mut last = segment.span.end();

    while parser.current_token().kind == TokenKind::ColonColon
        && parser.peek().kind == TokenKind::Identifier
    {
        parser.advance();
        let segment = parser.expect(TokenKind::Identifier)?;
        segments.push(Ident {
            value: SymbolExpr {
                value: segment.value,
            },
            span: segment.span,
        });

        last = segment.span.end();
    }

    Ok(Path {
        segments,
        span: Span::new(start.start(), last),
    })
}

pub fn parse_rename(parser: &mut Parser) -> Result<Option<Ident>> {
    if parser.current_token().kind == TokenKind::As {
        parser.advance();
        let rename = parser.expect(TokenKind::Identifier)?;
        Ok(Some(Ident {
            value: SymbolExpr {
                value: rename.value,
            },
            span: rename.span,
        }))
    } else {
        Ok(None)
    }
}
