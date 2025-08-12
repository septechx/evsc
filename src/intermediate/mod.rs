mod builtin;
mod compile_expr;
mod compile_type;
mod compiler;
mod emmiter;
mod pointer;

use anyhow::Result;
use inkwell::context::Context;

use crate::{ast::statements::BlockStmt, intermediate::compiler::CompilationContext};

pub fn compile(module_name: &str, ast: BlockStmt, path: &str) -> Result<()> {
    let context = Context::create();
    let module = context.create_module(module_name);
    let builder = context.create_builder();

    let mut compilation_context = CompilationContext::default();

    compiler::compile(&context, &module, &builder, &ast, &mut compilation_context)?;

    let output_name = module_name.strip_suffix(".evsc").unwrap_or(module_name);
    emmiter::emit_to_file(&format!("{path}/{output_name}.ll"), &module)?;

    Ok(())
}
