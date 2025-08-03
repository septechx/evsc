mod builtin;
mod compile_expr;
mod compiler;
mod emmiter;

use std::collections::HashMap;

use anyhow::Result;
use inkwell::{context::Context, types::BasicType, AddressSpace};

use crate::{
    ast::statements::BlockStmt,
    intermediate::compiler::{StructDef, SymbolTable, TypeContext},
};

pub fn compile(module_name: &str, ast: BlockStmt) -> Result<()> {
    let context = Context::create();
    let module = context.create_module(module_name);
    let builder = context.create_builder();

    let mut symbol_table = SymbolTable::new();
    let mut type_context = TypeContext {
        struct_defs: HashMap::new(),
    };

    compiler::compile(
        &context,
        &module,
        &builder,
        &ast,
        &mut symbol_table,
        &mut type_context,
    )?;

    let output_name = module_name.strip_suffix(".evsc").unwrap_or(module_name);
    emmiter::emit_to_file(&format!("_test/{output_name}.ll"), &module)?;

    Ok(())
}
