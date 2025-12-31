mod widgets;

use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use colored::Colorize;

use crate::errors::widgets::Widget;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorLevel {
    Warning,
    Error,
    Fatal,
}

impl Display for ErrorLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ErrorLevel::Warning => write!(f, "{}", "warning".yellow().bold()),
            ErrorLevel::Error => write!(f, "{}", "error".red().bold()),
            ErrorLevel::Fatal => write!(f, "{}", "fatal".red().bold()),
        }
    }
}

#[derive(Debug)]
pub struct CompilationError<'a> {
    level: ErrorLevel,
    message: Box<str>,
    widgets: Vec<Box<dyn Widget<Formatter<'a>>>>,
}

impl<'a> CompilationError<'a> {
    pub fn new(level: ErrorLevel, message: impl Into<Box<str>>) -> Self {
        Self {
            level,
            message: message.into(),
            widgets: Vec::new(),
        }
    }

    fn display_with_context(&self, f: &mut Formatter<'a>) -> fmt::Result {
        writeln!(f, "{}: {}", self.level, self.message.bold())?;

        for widget in &self.widgets {
            widget.render(f)?;
        }

        Ok(())
    }
}

impl<'a> Display for CompilationError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.display_with_context(f)
    }
}

#[derive(Debug)]
pub struct ErrorCollector {
    errors: Vec<CompilationError>,
    max_errors: usize,
    should_panic_on_fatal: bool,
}

impl ErrorCollector {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            max_errors: 100,
            should_panic_on_fatal: true,
        }
    }

    pub fn with_max_errors(mut self, max_errors: usize) -> Self {
        self.max_errors = max_errors;
        self
    }

    pub fn with_panic_on_fatal(mut self, should_panic: bool) -> Self {
        self.should_panic_on_fatal = should_panic;
        self
    }

    pub fn add(&mut self, error: CompilationError) {
        if error.level == ErrorLevel::Fatal && self.should_panic_on_fatal {
            eprintln!("{}", error);
            std::process::exit(1);
        }

        self.errors.push(error);

        if self.errors.len() >= self.max_errors {
            let max_error = builders::fatal(format!(
                "Too many errors ({}), stopping compilation",
                self.max_errors
            ));
            eprintln!("{}", max_error);
            std::process::exit(1);
        }
    }

    pub fn has_errors(&self) -> bool {
        self.errors.iter().any(|e| e.level >= ErrorLevel::Error)
    }

    pub fn has_warnings(&self) -> bool {
        self.errors.iter().any(|e| e.level == ErrorLevel::Warning)
    }

    pub fn get_errors(&self, min_level: ErrorLevel) -> Vec<&CompilationError> {
        self.errors
            .iter()
            .filter(|e| e.level >= min_level)
            .collect()
    }

    pub fn get_all_errors(&self) -> &[CompilationError] {
        &self.errors
    }

    pub fn clear(&mut self) {
        self.errors.clear();
    }

    pub fn print_all(&self) {
        for error in &self.errors {
            eprint!("{}", error);
        }
    }

    pub fn print_errors(&self, min_level: ErrorLevel) {
        for error in self.get_errors(min_level) {
            eprint!("{}", error);
        }
    }

    pub fn error_counts(&self) -> HashMap<ErrorLevel, usize> {
        let mut counts = HashMap::new();
        for error in &self.errors {
            *counts.entry(error.level).or_insert(0) += 1;
        }
        counts
    }

    pub fn can_continue(&self) -> bool {
        !self.errors.iter().any(|e| e.level == ErrorLevel::Fatal)
    }

    pub fn has_errors_above_level(&self, min_level: ErrorLevel) -> bool {
        self.errors.iter().any(|e| e.level >= min_level)
    }
}

impl Default for ErrorCollector {
    fn default() -> Self {
        Self::new()
    }
}

pub mod builders {
    use super::*;

    pub fn info(message: impl Into<String>) -> CompilationError {
        CompilationError::new(ErrorLevel::Info, message.into())
    }

    pub fn warning(message: impl Into<String>) -> CompilationError {
        CompilationError::new(ErrorLevel::Warning, message.into())
    }

    pub fn error(message: impl Into<String>) -> CompilationError {
        CompilationError::new(ErrorLevel::Error, message.into())
    }

    pub fn fatal(message: impl Into<String>) -> CompilationError {
        CompilationError::new(ErrorLevel::Fatal, message.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    #[test]
    fn test_error_levels() {
        assert!(ErrorLevel::Info < ErrorLevel::Warning);
        assert!(ErrorLevel::Warning < ErrorLevel::Error);
        assert!(ErrorLevel::Error < ErrorLevel::Fatal);
    }

    #[test]
    fn test_error_collector() {
        let mut collector = ErrorCollector::new();

        collector.add(CompilationError::new(
            ErrorLevel::Info,
            "Info message".to_string(),
        ));
        collector.add(CompilationError::new(
            ErrorLevel::Warning,
            "Warning message".to_string(),
        ));
        collector.add(CompilationError::new(
            ErrorLevel::Error,
            "Error message".to_string(),
        ));

        assert_eq!(collector.get_all_errors().len(), 3);
        assert!(collector.has_errors());
        assert!(collector.has_warnings());
        assert!(collector.can_continue());
    }

    #[test]
    fn test_error_with_span() {
        let span = Span::new(100, 105);
        let module_id = crate::SOURCE_MAPS.with(|sm| {
            let mut maps = sm.borrow_mut();
            maps.add_source(
                "test source content\nline 2\nline 3".to_string(),
                PathBuf::from("test.evsc"),
            )
        });

        let error = CompilationError::new(ErrorLevel::Error, "Test error".to_string())
            .with_span(span, module_id);

        assert!(error.location.is_some());
        if let Some(loc) = &error.location {
            assert_eq!(loc.span, span);
            assert_eq!(loc.module_id, module_id);
        }
    }
}
