use anyhow::{anyhow, bail, Result};
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
        let parameters = match &expr.kind {
            ExprKind::FunctionCall {
                callee: _,
                parameters,
            } => parameters,
            _ => unreachable!(),
        };

        if parameters.is_empty() {
            bail!("sizeof expects one type argument");
        }

        let ty = match &parameters[0].kind {
            ExprKind::Type(ty) => ty,
            _ => bail!("First argument must be a type expression"),
        };

        let llvm_ty = compile_type(context, ty, compilation_context)?;

        let size = llvm_ty
            .size_of()
            .ok_or_else(|| anyhow!("Could not get size of type: {llvm_ty:#?}"))?;

        Ok(SmartValue::from_value(size.as_basic_value_enum()))
    }
}
