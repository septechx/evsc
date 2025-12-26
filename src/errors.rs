use colored::*;
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    path::PathBuf,
    process,
};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file: PathBuf,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

impl SourceLocation {
    pub fn new(file: PathBuf, line: usize, column: usize, length: usize) -> Self {
        Self {
            file,
            line,
            column,
            length,
        }
    }

    pub fn simple(file: PathBuf, line: usize, column: usize) -> Self {
        Self::new(file, line, column, 1)
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file.display(), self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub struct InfoBlock {
    pub title: String,
}

impl InfoBlock {
    pub fn new(title: String) -> Self {
        Self { title }
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
    pub fn new(line: usize, code: String, code_type: CodeType) -> Self {
        Self {
            line,
            code,
            code_type,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilationError {
    pub level: ErrorLevel,
    pub message: String,
    pub location: Option<SourceLocation>,
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

    pub fn with_location(mut self, location: SourceLocation) -> Self {
        self.location = Some(location);
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

        if let Some(location) = &self.location {
            if let Some(code) = &self.code {
                writeln!(
                    f,
                    "{}{} {}",
                    " ".repeat(code.line.to_string().len()),
                    "-->".purple(),
                    location
                )?;
            } else {
                writeln!(f, " {} {}", "-->".purple(), location)?;
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
            if let Some(location) = &self.location {
                let underline = if location.length > 1 {
                    " ".repeat(location.column - 1) + &"^".repeat(location.length)
                } else {
                    " ".repeat(location.column - 1) + "^"
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
            panic!("{}", error);
        }

        self.errors.push(error);

        if self.errors.len() >= self.max_errors {
            let max_error = CompilationError::new(
                ErrorLevel::Fatal,
                format!(
                    "Too many errors ({}), stopping compilation",
                    self.max_errors
                ),
            );
            panic!("{}", max_error);
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

    pub fn error_at(
        message: impl Into<String>,
        file: PathBuf,
        line: usize,
        column: usize,
    ) -> CompilationError {
        CompilationError::new(ErrorLevel::Error, message.into())
            .with_location(SourceLocation::simple(file, line, column))
    }

    pub fn warning_at(
        message: impl Into<String>,
        file: PathBuf,
        line: usize,
        column: usize,
    ) -> CompilationError {
        CompilationError::new(ErrorLevel::Warning, message.into())
            .with_location(SourceLocation::simple(file, line, column))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

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
    fn test_error_with_location() {
        let location = SourceLocation::new(PathBuf::from("test.evsc"), 10, 5, 1);

        let error = CompilationError::new(ErrorLevel::Error, "Test error".to_string())
            .with_location(location);

        assert!(error.location.is_some());
    }

    #[test]
    fn test_info_block() {
        let info = InfoBlock::new("Type mismatch".to_string());

        assert_eq!(info.title, "Type mismatch");
    }
}
