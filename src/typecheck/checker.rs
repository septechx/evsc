use std::path::PathBuf;

use crate::{
    ast::Statement,
    lexer::token::Token,
    typecheck::{CheckOptions, Stage, stage1::Stage1},
};

pub struct TypeChecker {
    file_path: PathBuf,
    tokens: Vec<Token>,
    options: CheckOptions,
}

impl TypeChecker {
    pub fn new(file_path: PathBuf, tokens: Vec<Token>, options: CheckOptions) -> Self {
        Self {
            file_path,
            tokens,
            options,
        }
    }

    pub fn check(&self, statements: &[Statement]) {
        let stages: [Box<dyn Stage>; _] = [Box::new(Stage1::new(self.options.clone()))];

        for stage in stages.iter() {
            stage.run_checks(statements);
        }
    }
}
