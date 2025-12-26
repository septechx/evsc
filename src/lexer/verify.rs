use crate::{
    errors::{CodeLine, CodeType, builders},
    lexer::token::{Token, TokenKind},
};

pub fn verify_tokens(tokens: &[Token]) {
    for token in tokens {
        if let TokenKind::Illegal = &token.kind {
            let line = build_line_with_positions(tokens, token.location.line);
            let c = token.value.chars().next().unwrap_or('\0');

            crate::ERRORS.with(|e| {
                e.collector.borrow_mut().add(
                    builders::fatal(format!("Illegal token: {c}"))
                        .with_location(token.location.clone())
                        .with_code(CodeLine::new(token.location.line, line, CodeType::None)),
                );
            });
        }
    }
}

pub fn build_line_with_positions(tokens: &[Token], target_line: usize) -> String {
    let line_tokens: Vec<_> = tokens
        .iter()
        .filter(|t| t.location.line == target_line)
        .collect();

    if line_tokens.is_empty() {
        return String::new();
    }

    let mut sorted_tokens: Vec<_> = line_tokens.iter().collect();
    sorted_tokens.sort_by_key(|t| t.location.column);

    let mut line = String::new();
    let mut current_pos = 1;

    for token in sorted_tokens {
        let token_str = token.value.clone();
        let token_start = token.location.column;

        while current_pos < token_start {
            line.push(' ');
            current_pos += 1;
        }

        if token.kind == TokenKind::StringLiteral {
            line.push_str(&format!("\"{token_str}\""));
            current_pos += token_str.len() + 2;
        } else {
            line.push_str(&token_str);
            current_pos += token_str.len();
        }
    }

    line
}
