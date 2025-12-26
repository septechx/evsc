use crate::{
    errors::{CodeLine, CodeType, SourceLocation, builders},
    lexer::{token::Token, verify::build_line_with_positions},
};

pub fn process_string(str: &str, location: SourceLocation, tokens: &[Token]) -> String {
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
                _ => crate::ERRORS.with(|e| {
                    e.collector.borrow_mut().add(
                        builders::warning(format!("Unknown escape sequence \\{c}"))
                            .with_location(SourceLocation::new(
                                location.file.clone(),
                                location.line,
                                location.column + i,
                                2,
                            ))
                            .with_code(CodeLine::new(
                                location.line,
                                build_line_with_positions(tokens, location.line),
                                CodeType::None,
                            )),
                    );
                }),
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
