use crate::{
    ast::{Attribute, Statement},
    errors::builders,
    typecheck::Check,
};

#[derive(Default)]
pub struct AttributeValidator;

impl AttributeValidator {
    const KNOWN_ATTRIBUTES: &'static [&'static str] = &["internal"];
}

impl Check for AttributeValidator {
    fn check(&self, ast: &[Statement]) {
        for stmt in ast {
            match stmt {
                Statement::FnDecl(decl) => self.validate_attributes(&decl.attributes),
                Statement::StructDecl(decl) => self.validate_attributes(&decl.attributes),
                Statement::Block(block) => self.check(&block.body),
                _ => {}
            }
        }
    }
}

impl AttributeValidator {
    fn validate_attributes(&self, attrs: &[Attribute]) {
        for attr in attrs.iter() {
            if !Self::KNOWN_ATTRIBUTES.contains(&attr.name.as_str()) {
                crate::ERRORS.with(|e| {
                    e.collector.borrow_mut().add(
                        builders::warning(format!(
                            "Unknown attribute '{}'. Known attributes: {}",
                            attr.name,
                            Self::KNOWN_ATTRIBUTES.join(", ")
                        ))
                        .with_location(attr.location.clone()),
                    );
                });
            }
        }
    }
}
