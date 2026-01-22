use crate::{
    fatal_at,
    lexer::token::{TokenKind, TokenStream},
};

pub fn verify_tokens(tokens: &TokenStream) {
    for token in tokens.as_slice() {
        if let TokenKind::Illegal = &token.kind {
            let c = token.value.chars().next().unwrap_or('\0');
            fatal_at!(token.span, token.module_id, format!("Illegal token: {c}"));
        }
    }
}
