mod builtin;
mod compile_expr;
mod compiler;
mod emmiter;

use anyhow::Result;
use inkwell::context::Context;

use crate::ast::statements::BlockStmt;

pub fn compile(module_name: &str, ast: BlockStmt) -> Result<()> {
    let context = Context::create();
    let module = context.create_module(module_name);
    let builder = context.create_builder();

    compiler::compile(&context, &module, &builder, &ast)?;

    let output_name = module_name.strip_suffix(".evsc").unwrap_or(module_name);
    emmiter::emit_to_file(&format!("_test/{output_name}.ll"), &module)?;

    Ok(())
}
