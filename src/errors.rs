use colored::*;
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    path::PathBuf,
};

use crate::span::{ModuleId, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorLevel {
    Info,
    Warning,
    Error,
    Fatal,
}

impl Display for ErrorLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ErrorLevel::Info => write!(f, "{}", "INFO".blue().bold()),
            ErrorLevel::Warning => write!(f, "{}", "WARNING".yellow().bold()),
            ErrorLevel::Error => write!(f, "{}", "ERROR".red().bold()),
            ErrorLevel::Fatal => write!(f, "{}", "FATAL".red().bold()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocatedSpan {
    pub span: Span,
    pub module_id: ModuleId,
}

#[derive(Debug, Clone)]
pub struct InfoBlock {
    pub title: String,
}

impl InfoBlock {
    pub fn new(title: impl Into<String>) -> Self {
        Self {
            title: title.into(),
        }
    }
}

impl Display for InfoBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.title)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CodeType {
    Add,
    Remove,
    None,
}

#[derive(Debug, Clone)]
pub struct CodeLine {
    pub line: usize,
    pub code: String,
    pub code_type: CodeType,
}

impl CodeLine {
    pub fn new(line: usize, code: impl Into<String>, code_type: CodeType) -> Self {
        Self {
            line,
            code_type,
            code: code.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilationError {
    pub level: ErrorLevel,
    pub message: String,
    pub location: Option<LocatedSpan>,
    pub info_blocks: Vec<InfoBlock>,
    pub code: Option<CodeLine>,
}

impl CompilationError {
    pub fn new(level: ErrorLevel, message: String) -> Self {
        Self {
            level,
            message,
            location: None,
            info_blocks: Vec::new(),
            code: None,
        }
    }

    pub fn with_span(mut self, span: Span, module_id: ModuleId) -> Self {
        self.location = Some(LocatedSpan { span, module_id });
        self
    }

    pub fn with_info(mut self, info: InfoBlock) -> Self {
        self.info_blocks.push(info);
        self
    }

    pub fn with_code(mut self, code: CodeLine) -> Self {
        self.code = Some(code);
        self
    }

    fn display_with_context(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}: {}", self.level, self.message.bold())?;

        if let Some(located_span) = &self.location {
            let (file, ..) = crate::SOURCE_MAPS.with(|sm| {
                let maps = sm.borrow();
                maps.get_source(located_span.module_id)
                    .map(|sm| sm.span_to_source_location(&located_span.span))
                    .unwrap_or((PathBuf::from("unknown"), 0, 0, 0))
            });

            if let Some(code) = &self.code {
                writeln!(
                    f,
                    "{}{} {}",
                    " ".repeat(code.line.ilog10() as usize + 1),
                    "-->".purple(),
                    file.display()
                )?;
            } else {
                writeln!(f, " {} {}", "-->".purple(), file.display())?;
            }
        }

        if let Some(code) = &self.code {
            let line = code.line.to_string();
            writeln!(f, "{} {}", " ".repeat(line.len()), "|".purple())?;
            writeln!(
                f,
                "{} {} {}",
                line.purple(),
                "|".purple(),
                match code.code_type {
                    CodeType::Add => code.code.green(),
                    CodeType::Remove => code.code.red(),
                    CodeType::None => code.code.normal(),
                }
            )?;

            if let Some(located_span) = &self.location {
                let (_, _, column, length) = crate::SOURCE_MAPS.with(|sm| {
                    let maps = sm.borrow();
                    maps.get_source(located_span.module_id)
                        .map(|sm| sm.span_to_source_location(&located_span.span))
                        .unwrap_or((PathBuf::new(), 0, 0, 0))
                });

                let underline = if length > 1 {
                    " ".repeat(column - 1) + &"^".repeat(length)
                } else {
                    " ".repeat(column - 1) + "^"
                };
                writeln!(
                    f,
                    "{} {} {}",
                    " ".repeat(line.len()),
                    "|".purple(),
                    underline.red().bold()
                )?;
            } else {
                writeln!(f, "{} {}", " ".repeat(line.len()), "|".purple())?;
            }
        }

        if let Some(code) = &self.code {
            for info in &self.info_blocks {
                writeln!(
                    f,
                    "{} {} note: {}",
                    " ".repeat(code.line.to_string().len()),
                    "=".purple(),
                    info
                )?;
            }
        }

        Ok(())
    }
}

impl Display for CompilationError {
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
