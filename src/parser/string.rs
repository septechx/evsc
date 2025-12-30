use crate::{
    errors::builders,
    span::{ModuleId, Span},
};

pub fn process_string(str: &str, span: Span, module_id: ModuleId) -> String {
    let mut builder = String::new();

    let mut escaped = false;
    for (i, c) in str.chars().enumerate() {
        if escaped {
            match c {
                'n' => builder.push('\n'),
                'r' => builder.push('\r'),
                't' => builder.push('\t'),
                '0' => builder.push('\0'),
                '\\' => builder.push('\\'),
                _ => {
                    let error_span =
                        Span::new(span.start() + i as u32, span.start() + (i + 2) as u32);
                    crate::ERRORS.with(|e| {
                        e.borrow_mut().add(
                            builders::warning(format!("Unknown escape sequence \\{c}"))
                                .with_span(error_span, module_id),
                        );
                    });
                }
            }

            escaped = false;
            continue;
        }

        if c == '\\' {
            escaped = true;
            continue;
        }

        builder.push(c);
    }

    builder
}
