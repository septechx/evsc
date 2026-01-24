use std::collections::HashMap;

pub type Symbol = u32;

#[derive(Debug, Default, Clone)]
pub struct Interner {
    map: HashMap<String, Symbol>,
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
