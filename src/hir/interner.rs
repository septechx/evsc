use crate::hashmap::FxHashMap;

pub type Symbol = u32;

#[derive(Debug, Default, Clone)]
pub struct Interner {
    map: FxHashMap<String, Symbol>,
    vec: Vec<String>,
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.map.get(s) {
            return sym;
        }
        let idx = self.vec.len() as u32;
        self.vec.push(s.to_owned());
        self.map.insert(self.vec.last().unwrap().clone(), idx);
        idx
    }

    pub fn lookup(&self, sym: Symbol) -> &str {
        &self.vec[sym as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern_identical_strings() {
        let mut interner = Interner::new();
        let sym1 = interner.intern("hello");
        let sym2 = interner.intern("hello");
        assert_eq!(sym1, sym2);
    }

    #[test]
    fn test_intern_different_strings() {
        let mut interner = Interner::new();
        let sym1 = interner.intern("hello");
        let sym2 = interner.intern("world");
        assert_ne!(sym1, sym2);
    }

    #[test]
    fn test_lookup() {
        let mut interner = Interner::new();
        let sym = interner.intern("hello");
        assert_eq!(interner.lookup(sym), "hello");
    }

    #[test]
    fn test_intern_multiple_lookups() {
        let mut interner = Interner::new();
        let s1 = "apple";
        let s2 = "banana";
        let s3 = "cherry";

        let sym1 = interner.intern(s1);
        let sym2 = interner.intern(s2);
        let sym3 = interner.intern(s3);

        assert_eq!(interner.lookup(sym1), s1);
        assert_eq!(interner.lookup(sym2), s2);
        assert_eq!(interner.lookup(sym3), s3);

        assert_eq!(interner.intern(s1), sym1);
        assert_eq!(interner.intern(s2), sym2);
        assert_eq!(interner.intern(s3), sym3);
    }
}
