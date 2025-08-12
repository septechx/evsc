use std::fs;

use anyhow::{bail, Result};
use inkwell::context::Context;

use crate::{
    ast::{ast::Expression, expressions::FunctionCallExpr},
    intermediate::{
        compiler::{self, CompilationContext},
        pointer::SmartValue,
    },
    lexer::lexer::tokenize,
    parser::parser::parse,
};

pub fn import_module<'ctx>(
    _context: &'ctx Context,
    expr: &FunctionCallExpr,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    if expr.arguments.len() != 1 {
        bail!("Expected one argument to @import");
    }

    let module_name = match &expr.arguments[0] {
        Expression::String(sym) => sym.value.clone(),
        _ => bail!("Expected string literal as argument to @import"),
    };

    let module_path = compilation_context
        .module_path
        .parent()
        .unwrap()
        .join(&module_name);

    let file = fs::read_to_string(&module_path)?;

    let tokens = tokenize(file)?;
    let ast = parse(tokens)?;

    let context = Context::create();
    let module = context.create_module(&module_name);
    let builder = context.create_builder();

    let mut compilation_context = CompilationContext::new(module_path);

    compiler::compile(&context, &module, &builder, &ast, &mut compilation_context)?;

    // TODO: Parse ast to define a struct

    // TODO: Instantiate the module struct

    println!("{}: {:#?}", &module_name, &compilation_context);

    unimplemented!()
}
