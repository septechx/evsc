use std::collections::HashSet;

use crate::{ast::Statement, errors::builders, typecheck::Check};

#[derive(Default)]
pub struct DuplicateDeclarationChecker;

impl Check for DuplicateDeclarationChecker {
    fn check(&self, ast: &[Statement]) {
        let mut seen_functions: HashSet<String> = HashSet::new();
        let mut seen_structs: HashSet<String> = HashSet::new();

        self.collect_declarations(ast, &mut seen_functions, &mut seen_structs);
    }
}

impl DuplicateDeclarationChecker {
    fn collect_declarations(
        &self,
        ast: &[Statement],
        seen_functions: &mut HashSet<String>,
        seen_structs: &mut HashSet<String>,
    ) {
        for stmt in ast {
            match stmt {
                Statement::FnDecl(decl) => {
                    if seen_structs.contains(&decl.name) {
                        crate::ERRORS.with(|e| {
                            e.collector.borrow_mut().add(
                                builders::error(format!(
                                    "Function '{}' has the same name as a struct",
                                    decl.name
                                ))
                                .with_location(decl.location.clone()),
                            );
                        });
                    } else if !seen_functions.insert(decl.name.clone()) {
                        crate::ERRORS.with(|e| {
                            e.collector.borrow_mut().add(
                                builders::error(format!(
                                    "Duplicate function declaration: '{}'",
                                    decl.name
                                ))
                                .with_location(decl.location.clone()),
                            );
                        });
                    }
                }
                Statement::StructDecl(decl) => {
                    if seen_functions.contains(&decl.name) {
                        crate::ERRORS.with(|e| {
                            e.collector.borrow_mut().add(
                                builders::error(format!(
                                    "Struct '{}' has the same name as a function",
                                    decl.name
                                ))
                                .with_location(decl.location.clone()),
                            );
                        });
                    } else if !seen_structs.insert(decl.name.clone()) {
                        crate::ERRORS.with(|e| {
                            e.collector.borrow_mut().add(
                                builders::error(format!(
                                    "Duplicate struct declaration: '{}'",
                                    decl.name
                                ))
                                .with_location(decl.location.clone()),
                            );
                        });
                    }
                }
                Statement::Block(block) => {
                    self.collect_declarations(&block.body, seen_functions, seen_structs);
                }
                _ => {}
            }
        }
    }
}
