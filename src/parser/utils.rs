use crate::{
    errors::{CodeType, builders},
    lexer::token::Token,
    span::sourcemaps::get_code_line,
};

pub fn unexpected_token(token: Token) -> ! {
    let span = token.span;
    let module_id = token.module_id;

    let code_line = get_code_line(module_id, span, CodeType::None);

    crate::ERRORS.with(|e| {
        e.borrow_mut().add(
            builders::fatal(format!("Syntax error: Unexpected token `{}`", token.value))
                .with_span(span, module_id)
                .with_code(code_line),
        );
    });

    unreachable!()
}
