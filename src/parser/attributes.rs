use crate::{ast::Attribute, lexer::token::TokenKind, parser::Parser, span::Span};
use anyhow::Result;
use thin_vec::ThinVec;

pub fn parse_attributes(parser: &mut Parser) -> Result<ThinVec<Attribute>> {
    let mut attributes = ThinVec::new();

    while parser.current_token().kind == TokenKind::Hash {
        let hash_token = parser.advance();
        parser.expect(TokenKind::OpenBracket)?;

        let name = parser.expect_identifier()?;

        let parameters = if parser.current_token().kind == TokenKind::OpenParen {
            parser.advance();
            let mut args = ThinVec::new();

            while parser.current_token().kind != TokenKind::CloseParen {
                args.push(parser.expect(TokenKind::Identifier)?.value);

                if parser.current_token().kind != TokenKind::CloseParen {
                    parser.expect(TokenKind::Comma)?;
                }
            }

            parser.expect(TokenKind::CloseParen)?;
            Some(args)
        } else {
            None
        };

        let close_token = parser.expect(TokenKind::CloseBracket)?;

        let span = Span::new(hash_token.span.start(), close_token.span.end());

        attributes.push(Attribute {
            parameters,
            name,
            span,
        });
    }

    Ok(attributes)
}

#[macro_export]
macro_rules! no_attributes {
    ($parser:expr, $modifiers:expr) => {{
        let parser = $parser;
        let attributes = $modifiers;

        if !attributes.is_empty() {
            error_at!(
                attributes[0].span,
                parser.current_token().module_id,
                "Attribute not allowed here"
            )?;
        }
    }};
}
