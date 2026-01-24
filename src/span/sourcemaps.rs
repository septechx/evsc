use std::{collections::HashMap, path::PathBuf};

use crate::span::{PackageId, Span};

#[derive(Debug, Clone)]
pub struct SourceMap {
    content: Box<str>,
    path: PathBuf,
    line_starts: Vec<u32>,
}

impl SourceMap {
    pub fn new(content: String, path: PathBuf) -> Self {
        let mut line_starts = vec![0u32];
        for (i, c) in content.char_indices() {
            if c == '\n' {
                line_starts.push((i + 1) as u32);
            }
        }
        Self {
            content: content.into_boxed_str(),
            path,
            line_starts,
        }
    }

    pub fn line_column(&self, byte_offset: u32) -> (usize, usize) {
        let offset = byte_offset as usize;
        if offset >= self.content.len() {
            return (self.line_starts.len(), 0);
        }

        let result = self.line_starts.binary_search(&(offset as u32));
        let line_idx = match result {
            Ok(i) => i,
            Err(i) => i - 1,
        };

        let line_start = self.line_starts[line_idx] as usize;
        let column = offset.saturating_sub(line_start);
        (line_idx + 1, column + 1)
    }

    pub fn get_line(&self, line: usize) -> Option<&str> {
        if line == 0 || line > self.line_starts.len() {
            return None;
        }
        let start = self.line_starts[line - 1] as usize;
        let end = if line < self.line_starts.len() {
            self.line_starts[line] as usize
        } else {
            self.content.len()
        };
        let line_content = self.content.get(start..end)?;
        if let Some(stripped) = line_content.strip_suffix('\n') {
            Some(stripped)
        } else {
            Some(line_content)
        }
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn span_to_source_location(&self, span: &Span) -> (PathBuf, usize, usize, usize) {
        let (line, column) = self.line_column(span.start());
        (self.path.clone(), line, column, span.len() as usize)
    }
}

#[derive(Debug, Clone, Default)]
pub struct SourceMapManager {
    source_maps: HashMap<PackageId, SourceMap>,
    next_id: PackageId,
}

impl SourceMapManager {
    pub fn add_source(&mut self, content: String, path: PathBuf) -> PackageId {
        let id = self.next_id;
        let source_map = SourceMap::new(content, path);
        self.source_maps.insert(id, source_map);
        self.next_id = PackageId(self.next_id.0 + 1);
        id
    }

    pub fn next_id(&mut self) -> PackageId {
        let id = self.next_id;
        self.next_id = PackageId(self.next_id.0 + 1);
        id
    }

    pub fn get_source(&self, id: PackageId) -> Option<&SourceMap> {
        self.source_maps.get(&id)
    }

    pub fn get_line_column(&self, id: PackageId, offset: u32) -> Option<(usize, usize)> {
        self.source_maps.get(&id).map(|sm| sm.line_column(offset))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_column_simple() {
        let content = "hello\nworld\n";
        let sm = SourceMap::new(content.to_string(), PathBuf::from("test.oxi"));

        assert_eq!(sm.line_column(0), (1, 1));
        assert_eq!(sm.line_column(5), (1, 6));
        assert_eq!(sm.line_column(6), (2, 1));
        assert_eq!(sm.line_column(11), (2, 6));
    }

    #[test]
    fn test_line_column_multiline() {
        let content = "first line\nsecond line\nthird line";
        let sm = SourceMap::new(content.to_string(), PathBuf::from("test.oxi"));

        assert_eq!(sm.line_column(0), (1, 1));
        assert_eq!(sm.line_column(11), (2, 1));
        assert_eq!(sm.line_column(23), (3, 1));
    }

    #[test]
    fn test_get_line() {
        let content = "line1\nline2\nline3";
        let sm = SourceMap::new(content.to_string(), PathBuf::from("test.oxi"));

        assert_eq!(sm.get_line(1), Some("line1"));
        assert_eq!(sm.get_line(2), Some("line2"));
        assert_eq!(sm.get_line(3), Some("line3"));
        assert_eq!(sm.get_line(4), None);
    }

    #[test]
    fn test_span_to_source_location() {
        let content = "hello world\n";
        let sm = SourceMap::new(content.to_string(), PathBuf::from("test.oxi"));
        let span = Span::new(0, 5);

        let (path, line, column, length) = sm.span_to_source_location(&span);
        assert_eq!(path.as_os_str(), "test.oxi");
        assert_eq!(line, 1);
        assert_eq!(column, 1);
        assert_eq!(length, 5);
    }
}
