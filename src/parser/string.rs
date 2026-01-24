use crate::{
    span::{PackageId, Span},
    warning_at,
};

pub fn process_string(str: &str, span: Span, module_id: PackageId) -> String {
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
                    let _ = warning_at!(
                        error_span,
                        module_id,
                        format!("Unknown escape sequence \\{c}")
                    );
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
