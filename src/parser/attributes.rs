use crate::{ast::Attribute, lexer::token::TokenKind, parser::Parser, span::Span};
use anyhow::Result;

pub fn parse_attributes(parser: &mut Parser) -> Result<Vec<Attribute>> {
    let mut attributes = Vec::new();

    while parser.current_token().kind == TokenKind::Hash {
        let hash_token = parser.advance();
        parser.expect(TokenKind::OpenBracket)?;

        let name = parser.expect_identifier()?;

        let arguments = if parser.current_token().kind == TokenKind::OpenParen {
            parser.advance();
            let mut args = Vec::new();

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
            name,
            arguments,
            span,
        });
    }

    Ok(attributes)
}
