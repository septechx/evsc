use std::fmt::{Display, Formatter};

use crate::elogln;

pub mod sourcemaps;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct PackageId(pub u32);

impl Display for PackageId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    low: u32,
    len: u32,
}

impl Span {
    pub fn new(mut low: u32, mut high: u32) -> Self {
        if low > high {
            elogln!("Warning: Span created with low > high: {} > {}", low, high);
            std::mem::swap(&mut low, &mut high);
        }

        Self {
            low,
            len: (high - low),
        }
    }

    pub fn shrink_to_start(&self) -> Self {
        Self {
            low: self.low,
            len: 0,
        }
    }

    pub fn start(&self) -> u32 {
        self.low
    }

    pub fn end(&self) -> u32 {
        self.low + self.len
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn contains(&self, offset: u32) -> bool {
        offset >= self.low && offset <= self.end()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_methods() {
        let span = Span::new(10, 20);
        assert_eq!(span.start(), 10);
        assert_eq!(span.end(), 20);
        assert_eq!(span.len(), 10);
        assert!(!span.contains(5));
        assert!(span.contains(10));
        assert!(span.contains(15));
        assert!(!span.contains(21));
    }
}
