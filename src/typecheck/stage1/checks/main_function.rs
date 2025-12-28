use crate::{
    ast::{Statement, Type, types::SymbolType},
    errors::{InfoBlock, SourceLocation, builders},
    typecheck::{Check, CheckOptions},
};

pub struct MainFunctionChecker {
    options: CheckOptions,
}

impl MainFunctionChecker {
    pub fn new(options: CheckOptions) -> Self {
        Self { options }
    }
}

impl Check for MainFunctionChecker {
    fn check(&self, ast: &[Statement]) {
        if self.options.no_link {
            return;
        }

        let mut main_count = 0;
        let mut main_location = None;
        self.find_main_function(ast, &mut main_count, &mut main_location);

        if main_count == 0 {
            crate::ERRORS.with(|e| {
                e.collector
                    .borrow_mut()
                    .add(
                        builders::error("Main function not found").with_info(InfoBlock::new(
                            "Add a main function or compile with `--no-link`",
                        )),
                    );
            });
        } else if let Some(location) = main_location
            && main_count > 1
        {
            crate::ERRORS.with(|e| {
                e.collector
                    .borrow_mut()
                    .add(builders::error("Multiple main functions found").with_location(location));
            });
        }
    }
}

impl MainFunctionChecker {
    fn find_main_function(
        &self,
        ast: &[Statement],
        count: &mut usize,
        location: &mut Option<SourceLocation>,
    ) {
        for stmt in ast {
            match stmt {
                Statement::FnDecl(decl) => {
                    if decl.name == "main" && decl.is_public {
                        *count += 1;
                        if location.is_none() {
                            *location = Some(decl.location.clone());
                        }
                        if !self.is_valid_main_return_type(&decl.explicit_type) {
                            crate::ERRORS.with(|e| {
                                e.collector.borrow_mut().add(
                                    builders::error("Main function must return an integer or void")
                                        .with_location(decl.location.clone()),
                                );
                            });
                        }
                    }
                }
                Statement::Block(block) => {
                    self.find_main_function(&block.body, count, location);
                }
                _ => {}
            }
        }
    }

    fn is_valid_main_return_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Symbol(SymbolType { name }) if name == "void" => true,
            Type::Symbol(SymbolType { name }) => {
                matches!(
                    name.as_str(),
                    "i8" | "i16"
                        | "i32"
                        | "i64"
                        | "i128"
                        | "isize"
                        | "u8"
                        | "u16"
                        | "u32"
                        | "u64"
                        | "u128"
                        | "usize"
                )
            }
            _ => false,
        }
    }
}
