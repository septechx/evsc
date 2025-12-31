use std::{
    fmt::{Debug, Write},
    path::PathBuf,
};

use anyhow::Result;
use colored::Colorize;

use crate::span::{ModuleId, Span};

pub trait Widget<T: Write>: Debug {
    fn render(&self, f: &mut T) -> std::fmt::Result;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum HighlightType {
    Warning,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum CodeType {
    Add,
    Remove,
    Default,
}

#[derive(Debug, Clone)]
pub struct CodeWidget {
    span: Span,
    module_id: ModuleId,
    line: usize,
    column: usize,
    length: usize,
    code: Box<str>,
    code_type: CodeType,
    highlight_type: HighlightType,
}

impl CodeWidget {
    pub fn new(span: Span, module_id: ModuleId) -> Result<Self> {
        let (_, line, column, length) = crate::SOURCE_MAPS.with(|sm| {
            let maps = sm.borrow();
            maps.get_source(module_id)
                .map(|sm| sm.span_to_source_location(&span))
                .ok_or_else(|| anyhow::anyhow!("Source map not found for module id {module_id}"))
        })?;

        let code = crate::SOURCE_MAPS.with(|sm| {
            let maps = sm.borrow();
            maps.get_source(module_id)
                .and_then(|sm| sm.get_line(line))
                .unwrap_or("<failed to get line>")
                .to_string()
        });

        Ok(Self {
            span,
            module_id,
            line,
            column,
            length,
            code_type: CodeType::Default,
            highlight_type: HighlightType::Error,
            code: code.into(),
        })
    }

    pub fn from_raw(
        span: Span,
        module_id: ModuleId,
        line: usize,
        column: usize,
        length: usize,
        code: Box<str>,
        code_type: CodeType,
        highlight_type: HighlightType,
    ) -> Self {
        Self {
            span,
            module_id,
            line,
            column,
            length,
            code,
            code_type,
            highlight_type,
        }
    }

    pub fn code_type(&mut self, code_type: CodeType) -> &mut Self {
        self.code_type = code_type;
        self
    }

    pub fn highlight_type(&mut self, highlight_type: HighlightType) -> &mut Self {
        self.highlight_type = highlight_type;
        self
    }
}

impl<T: Write> Widget<T> for CodeWidget {
    fn render(&self, f: &mut T) -> std::fmt::Result {
        let pad = (self.line.ilog10() + 1) as usize;

        writeln!(f, "{} {}", " ".repeat(pad), "|".purple())?;
        writeln!(
            f,
            "{} {} {}",
            self.line.to_string().purple(),
            "|".purple(),
            match self.code_type {
                CodeType::Add => self.code.green(),
                CodeType::Remove => self.code.red(),
                CodeType::Default => self.code.normal(),
            }
        )?;

        let underline = if self.length > 1 {
            " ".repeat(self.column - 1) + &"^".repeat(self.length)
        } else {
            " ".repeat(self.column - 1) + "^"
        };
        write!(
            f,
            "{} {} {}",
            " ".repeat(pad),
            "|".purple(),
            match self.highlight_type {
                HighlightType::Warning => underline.yellow().bold(),
                HighlightType::Error => underline.red().bold(),
            }
        )?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LocationWidget {
    span: Span,
    module_id: ModuleId,
    line: usize,
    column: usize,
    file: PathBuf,
}

impl LocationWidget {
    pub fn new(span: Span, module_id: ModuleId) -> Result<Self> {
        let (file, line, column, _) = crate::SOURCE_MAPS.with(|sm| {
            let maps = sm.borrow();
            maps.get_source(module_id)
                .map(|sm| sm.span_to_source_location(&span))
                .ok_or_else(|| anyhow::anyhow!("Source map not found for module id {module_id}"))
        })?;

        Ok(Self {
            span,
            module_id,
            line,
            column,
            file,
        })
    }

    pub fn from_raw(
        span: Span,
        module_id: ModuleId,
        line: usize,
        column: usize,
        file: PathBuf,
    ) -> Self {
        Self {
            span,
            module_id,
            line,
            column,
            file,
        }
    }
}

impl<T: Write> Widget<T> for LocationWidget {
    fn render(&self, f: &mut T) -> std::fmt::Result {
        write!(
            f,
            "{}{} {}:{}:{}",
            " ".repeat(self.line.ilog10() as usize + 1),
            "-->".purple(),
            self.file.display(),
            self.line,
            self.column
        )?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct InfoWidget {
    span: Span,
    module_id: ModuleId,
    line: usize,
    content: Box<str>,
}

impl InfoWidget {
    pub fn new(span: Span, module_id: ModuleId, content: impl Into<Box<str>>) -> Result<Self> {
        let (_, line, ..) = crate::SOURCE_MAPS.with(|sm| {
            let maps = sm.borrow();
            maps.get_source(module_id)
                .map(|sm| sm.span_to_source_location(&span))
                .ok_or_else(|| anyhow::anyhow!("Source map not found for module id {module_id}"))
        })?;

        Ok(Self {
            span,
            module_id,
            line,
            content: content.into(),
        })
    }

    pub fn from_raw(span: Span, module_id: ModuleId, line: usize, content: Box<str>) -> Self {
        Self {
            span,
            module_id,
            line,
            content,
        }
    }
}

impl<T: Write> Widget<T> for InfoWidget {
    fn render(&self, f: &mut T) -> std::fmt::Result {
        write!(
            f,
            "{} {} note: {}",
            " ".repeat(self.line.to_string().len()),
            "=".purple(),
            self.content
        )?;

        Ok(())
    }
}
