use anyhow::Result;
use std::path::PathBuf;

use crate::{
    ast::{
        Statement,
        statements::{FnDeclStmt, StructDeclStmt},
    },
    errors::{CompilationError, ErrorLevel},
    lexer::token::Token,
};

pub struct TypeChecker {
    file_path: PathBuf,
    tokens: Vec<Token>,
}

// TODO: Add type checking
impl TypeChecker {
    pub fn new(file_path: PathBuf, tokens: Vec<Token>) -> Self {
        Self { file_path, tokens }
    }

    pub fn check(&self, statements: &[Statement]) -> Result<()> {
        for stmt in statements {
            match stmt {
                Statement::FnDecl(fn_decl) => {
                    self.check_fn_decl(fn_decl)?;
                }
                Statement::StructDecl(struct_decl) => {
                    self.check_struct_decl(struct_decl)?;
                }
                Statement::Block(block) => {
                    self.check(&block.body)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn check_fn_decl(&self, fn_decl: &FnDeclStmt) -> Result<()> {
        for attr in &fn_decl.attributes {
            self.check_attribute(attr, &fn_decl.name, "function")?;
        }
        Ok(())
    }

    fn check_struct_decl(&self, struct_decl: &StructDeclStmt) -> Result<()> {
        for attr in &struct_decl.attributes {
            self.check_attribute(attr, &struct_decl.name, "struct")?;
        }
        Ok(())
    }

    fn check_attribute(
        &self,
        attr: &crate::ast::Attribute,
        item_name: &str,
        item_type: &str,
    ) -> Result<()> {
        if attr.name == "internal" {
            self.check_internal_attribute(item_name, item_type)?;
        }
        Ok(())
    }

    fn check_internal_attribute(&self, _item_name: &str, _item_type: &str) -> Result<()> {
        let path_str = self.file_path.to_string_lossy();

        let is_builtin = path_str.contains("/builtin/")
            || path_str.ends_with("lib/builtin/lib.evsc")
            || path_str.contains("\\builtin\\")
            || path_str.ends_with("lib\\builtin\\lib.evsc");

        if !is_builtin {
            crate::ERRORS.with(|e| {
                e.collector.borrow_mut().add(
                    CompilationError::new(
                        ErrorLevel::Error,
                        "Cannot use `#[internal]` attribute in user code. This attribute is only allowed in builtin library files.".to_string(),
                    ),
                );
            });
        }

        Ok(())
    }
}
