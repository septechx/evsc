use anyhow::Result;

use crate::{
    errors::{
        builders,
        widgets::{CodeWidget, LocationWidget},
    },
    lexer::token::Token,
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
