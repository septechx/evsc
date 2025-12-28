mod options;
pub use options::CheckOptions;

mod stage1;

mod checker;
pub use checker::TypeChecker;

use crate::ast::Statement;

pub trait Check {
    fn check(&self, ast: &[Statement]);
}

pub trait Stage {
    fn run_checks(&self, ast: &[Statement]);
}
