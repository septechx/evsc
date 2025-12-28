use crate::{
    ast::{Attribute, Statement},
    errors::builders,
    typecheck::{Check, CheckOptions},
};

pub struct InternalAttributeChecker {
    _options: CheckOptions,
}

impl InternalAttributeChecker {
    pub fn new(options: CheckOptions) -> Self {
        Self { _options: options }
    }
}

impl Check for InternalAttributeChecker {
    fn check(&self, ast: &[Statement]) {
        for stmt in ast {
            match stmt {
                Statement::FnDecl(decl) => self.check_attributes(&decl.attributes),
                Statement::StructDecl(decl) => self.check_attributes(&decl.attributes),
                Statement::Block(block) => self.check(&block.body),
                _ => {}
            }
        }
    }
}

impl InternalAttributeChecker {
    fn check_attributes(&self, attrs: &[Attribute]) {
        for attr in attrs.iter() {
            if attr.name == "internal" {
                crate::ERRORS.with(|e| {
                e.collector.borrow_mut().add(
                    builders::warning(
                        "Cannot use `#[internal]` attribute in user code. This attribute is only allowed in builtin library files.",
                    ),
                );
            });
            }
        }
    }
}
