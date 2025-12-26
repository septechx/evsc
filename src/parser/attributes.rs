use crate::{ast::Attribute, lexer::token::TokenKind, parser::Parser};
use anyhow::Result;

pub fn parse_attributes(parser: &mut Parser) -> Result<Vec<Attribute>> {
    let mut attributes = Vec::new();

    while parser.current_token().kind == TokenKind::Hash {
        parser.advance();
        parser.expect(TokenKind::OpenBracket)?;

        let name = parser.expect(TokenKind::Identifier)?.value;

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

        parser.expect(TokenKind::CloseBracket)?;

        attributes.push(Attribute { name, arguments });
    }

    Ok(attributes)
}
