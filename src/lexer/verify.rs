use crate::{
    errors::{CodeLine, CodeType, builders},
    lexer::token::{TokenKind, TokenStream},
};

pub fn verify_tokens(tokens: &TokenStream) {
    for token in &tokens.0 {
        if let TokenKind::Illegal = &token.kind {
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

            let c = token.value.chars().next().unwrap_or('\0');

            crate::ERRORS.with(|e| {
                e.borrow_mut().add(
                    builders::fatal(format!("Illegal token: {c}"))
                        .with_span(token.span, token.module_id)
                        .with_code(CodeLine::new(line, line_content, CodeType::None)),
                );
            });
        }
    }
}
