use std::path::PathBuf;

use crate::{ast::Statement, typecheck::CheckOptions};

pub struct TypeChecker {
    file_path: PathBuf,
    options: CheckOptions,
}

impl TypeChecker {
    pub fn new(file_path: PathBuf, options: CheckOptions) -> Self {
        Self { file_path, options }
    }

    pub fn check(&self, statements: &[Statement]) {}
}
