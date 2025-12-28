use std::path::PathBuf;

use crate::{
    ast::Statement,
    lexer::token::Token,
    typecheck::{Stage, stage1::Stage1},
};

pub struct TypeChecker {
    file_path: PathBuf,
    tokens: Vec<Token>,
}

impl TypeChecker {
    pub fn new(file_path: PathBuf, tokens: Vec<Token>) -> Self {
        Self { file_path, tokens }
    }

    pub fn check(&self, statements: &[Statement]) {
        let stages: [Box<dyn Stage>; _] = [Box::new(Stage1::new())];

        for stage in stages.iter() {
            stage.run_checks(statements);
        }
    }
}
