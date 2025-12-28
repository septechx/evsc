mod checks;
use checks::*;

use crate::{
    ast::Statement,
    typecheck::{Check, CheckOptions, Stage},
};

pub struct Stage1 {
    checks: Vec<Box<dyn Check>>,
}

impl Stage1 {
    pub fn new(options: CheckOptions) -> Self {
        Self {
            checks: vec![
                Box::new(InternalAttributeChecker::new(options.clone())),
                Box::new(MainFunctionChecker::new(options.clone())),
                Box::new(DuplicateDeclarationChecker),
                Box::new(AttributeValidator),
            ],
        }
    }
}

impl Stage for Stage1 {
    fn run_checks(&self, ast: &[Statement]) {
        for check in &self.checks {
            check.check(ast);
        }
    }
}
