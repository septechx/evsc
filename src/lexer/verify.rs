use anyhow::Result;

use crate::{
    errors::{
        builders,
        widgets::{CodeWidget, LocationWidget},
    },
    lexer::token::{TokenKind, TokenStream},
};

pub fn verify_tokens(tokens: &TokenStream) {
    for token in &tokens.0 {
        if let TokenKind::Illegal = &token.kind {
            let c = token.value.chars().next().unwrap_or('\0');

            crate::ERRORS
                .with(|e| -> Result<()> {
                    e.borrow_mut().add(
                        builders::fatal(format!("Illegal token: {c}"))
                            .add_widget(LocationWidget::new(token.span, token.module_id)?)
                            .add_widget(CodeWidget::new(token.span, token.module_id)?),
                    );
                    Ok(())
                })
                .expect("failed to create error");
        }
    }
}
