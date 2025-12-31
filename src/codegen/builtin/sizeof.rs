use anyhow::{Result, anyhow, bail};
use inkwell::{context::Context, types::BasicType, values::BasicValue};

use crate::{
    ast::{Expr, ExprKind},
    codegen::{
        builtin::BuiltinFunction, compile_type::compile_type, compiler::CompilationContext,
        pointer::SmartValue,
    },
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct SizeofBuiltin;

impl BuiltinFunction for SizeofBuiltin {
    fn handle_call<'ctx>(
        context: &'ctx Context,
        _module: &inkwell::module::Module<'ctx>,
        _builder: &inkwell::builder::Builder<'ctx>,
        expr: &Expr,
        compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<SmartValue<'ctx>> {
        let expr = match &expr.kind {
            ExprKind::FunctionCall(expr) => expr,
            _ => unreachable!(),
        };

        let ty = match &expr.arguments[0].kind {
            ExprKind::Type(ty) => ty.underlying.clone(),
            _ => bail!("First argument must be a type expression"),
        };

        let llvm_ty = compile_type(context, &ty, compilation_context)?;

        let size = llvm_ty
            .size_of()
            .ok_or_else(|| anyhow!("Could not get size of type: {llvm_ty:#?}"))?;

        Ok(SmartValue::from_value(size.as_basic_value_enum()))
    }
}
