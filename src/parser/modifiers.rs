use std::fmt::{Display, Formatter};

use crate::{
    lexer::token::{Token, TokenKind},
    parser::Parser,
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModifierKind {
    Pub,
    Extern,
}

impl From<TokenKind> for ModifierKind {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Pub => Self::Pub,
            TokenKind::Extern => Self::Extern,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Modifier {
    pub kind: ModifierKind,
    pub span: Span,
}

impl Display for ModifierKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pub => write!(f, "pub"),
            Self::Extern => write!(f, "extern"),
        }
    }
}

impl From<Token> for Modifier {
    fn from(token: Token) -> Self {
        Self {
            kind: token.kind.into(),
            span: token.span,
        }
    }
}

pub fn parse_modifiers(parser: &mut Parser) -> Vec<Modifier> {
    let mut modifiers = Vec::new();

    while matches!(
        parser.current_token().kind,
        TokenKind::Pub | TokenKind::Extern
    ) {
        modifiers.push(parser.advance().into());
    }

    modifiers
}

/// Returns the total span of all modifiers
/// Requires that modifiers must not be empty and must be sorted by span
pub fn get_modifiers_span(modifiers: &[Modifier]) -> Span {
    Span::new(
        modifiers.first().unwrap().span.start(),
        modifiers.last().unwrap().span.end(),
    )
}

/// Validates and extracts modifiers
/// ```rust,ignore
/// let (pub_mod, extern_mod) = get_modifiers!(parser, modifiers, [Pub, Extern]);
/// ```
#[macro_export]
macro_rules! get_modifiers {
    ($parser:expr, $modifiers:expr, [$( $expected:ident ),* $(,)?]) => {{
        let modifiers = $modifiers;
        let expected_order = [$( ModifierKind::$expected ),*];

        if !modifiers.is_empty() && !expected_order.is_empty() {
            let span = get_modifiers_span(&modifiers);
            let code_line = get_code_line(
                $parser.current_token().module_id,
                span,
                CodeType::None,
            );

            for (idx, modifier) in modifiers.iter().enumerate() {
                if !expected_order.contains(&modifier.kind) {
                    ERRORS.with(|e| {
                        e.borrow_mut().add(
                            builders::error(format!(
                                "Unexpected modifier '{}'",
                                modifier.kind
                            ))
                            .with_span(modifier.span, $parser.current_token().module_id)
                            .with_code(code_line.clone()),
                        );
                    });
                }

                if modifiers.iter().enumerate().any(|(i, m)| i != idx && m.kind == modifier.kind) {
                   ERRORS.with(|e| {
                        e.borrow_mut().add(
                            builders::error(format!(
                                "Duplicate modifier '{}'",
                                modifier.kind
                            ))
                            .with_span(modifier.span, $parser.current_token().module_id)
                            .with_code(code_line.clone()),
                        );
                    });
                }

                if let Some(expected_idx) = expected_order.iter().position(|&e| e == modifier.kind) {
                    let mut prev_idx = None;
                    for prev_mod in modifiers.iter().take(idx) {
                        if let Some(pi) = expected_order.iter().position(|&e| e == prev_mod.kind) {
                            prev_idx = Some(pi);
                        }
                    }
                    if let Some(prev) = prev_idx {
                        if expected_idx < prev {
                            ERRORS.with(|e| {
                                e.borrow_mut().add(
                                    builders::error(format!(
                                        "Modifier '{}' must appear before '{}'",
                                        modifier.kind,
                                        expected_order[expected_idx.saturating_sub(1)]
                                    ))
                                    .with_span(modifier.span, $parser.current_token().module_id)
                                    .with_code(code_line.clone()),
                                );
                            });
                        }
                    }
                }
            }
        }

        (
            $(
                modifiers.iter().find(|m| m.kind == ModifierKind::$expected).copied(),
            )*
        )
    }}
}
