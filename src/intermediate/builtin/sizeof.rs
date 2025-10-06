use anyhow::{anyhow, bail, Result};
use inkwell::{context::Context, types::BasicType, values::BasicValue};

use crate::{
    ast::{ast::Expression, expressions::FunctionCallExpr},
    intermediate::{compile_type::compile_type, compiler::CompilationContext, pointer::SmartValue},
};

pub fn handle_sizeof_call<'ctx>(
    context: &'ctx Context,
    expr: &FunctionCallExpr,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    let ty = match &expr.arguments[0] {
        Expression::Type(ty) => ty.underlying.clone(),
        _ => bail!("First argument must be a type expression"),
    };

    let llvm_ty = compile_type(context, &ty, compilation_context);

    let size = llvm_ty
        .size_of()
        .ok_or_else(|| anyhow!("Could not get size of type: {llvm_ty:#?}"))?;

    Ok(SmartValue::from_value(size.as_basic_value_enum()))
}
