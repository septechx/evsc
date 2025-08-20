use crate::{
    errors::{CodeLine, ErrorLevel},
    lexer::token::LocatedToken,
    ERRORS,
};

pub fn verify_tokens(tokens: &[LocatedToken]) {
    for token in tokens {
        if let crate::lexer::token::Token::Illegal(c) = &token.token {
            let line = build_line_with_positions(tokens, token.location.line);

            ERRORS.lock().add_with_location_and_code(
                ErrorLevel::Fatal,
                format!("Illegal token: {c}"),
                token.location.clone(),
                CodeLine::new(token.location.line, line),
            );
        }
    }
}

fn build_line_with_positions(tokens: &[LocatedToken], target_line: usize) -> String {
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
        let token_str = token.token.to_string();
        let token_start = token.location.column;

        while current_pos < token_start {
            line.push(' ');
            current_pos += 1;
        }

        line.push_str(&token_str);
        current_pos += token_str.len();
    }

    line
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        errors::ErrorCollector,
        lexer::token::{LocatedToken, Token},
    };
    use std::path::PathBuf;

    #[test]
    fn test_verify_tokens_with_location() {
        let mut temp_errors = ErrorCollector::new().with_panic_on_fatal(false);

        let test_path = PathBuf::from("test.evsc");
        let illegal_token = LocatedToken::new_simple(Token::Illegal('@'), test_path.clone(), 5, 12);

        let tokens = vec![illegal_token];

        for token in &tokens {
            if let crate::lexer::token::Token::Illegal(c) = &token.token {
                temp_errors.add_with_location(
                    ErrorLevel::Fatal,
                    format!("Illegal token: {c}"),
                    token.location.clone(),
                );
            }
        }

        let all_errors = temp_errors.get_all_errors();
        assert_eq!(all_errors.len(), 1);

        let error = &all_errors[0];
        assert_eq!(error.level, ErrorLevel::Fatal);
        assert_eq!(error.message, "Illegal token: @");

        if let Some(location) = &error.location {
            assert_eq!(location.file, test_path);
            assert_eq!(location.line, 5);
            assert_eq!(location.column, 12);
        } else {
            panic!("Error should have location information");
        }
    }
}
