use crate::{
    errors::{CodeType, builders},
    lexer::token::{TokenKind, TokenStream},
    span::sourcemaps::get_code_line,
};

pub fn verify_tokens(tokens: &TokenStream) {
    for token in &tokens.0 {
        if let TokenKind::Illegal = &token.kind {
            let code_line = get_code_line(token.module_id, token.span, CodeType::None);
            let c = token.value.chars().next().unwrap_or('\0');

            crate::ERRORS.with(|e| {
                e.borrow_mut().add(
                    builders::fatal(format!("Illegal token: {c}"))
                        .with_span(token.span, token.module_id)
                        .with_code(code_line),
                );
            });
        }
    }
}
