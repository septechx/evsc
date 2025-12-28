mod attribute_validator;
mod duplicate_declaration;
mod internal_attribute;
mod main_function;

pub use attribute_validator::AttributeValidator;
pub use duplicate_declaration::DuplicateDeclarationChecker;
pub use internal_attribute::InternalAttributeChecker;
pub use main_function::MainFunctionChecker;
