use anyhow::{Result, anyhow, bail};
use inkwell::{
    InlineAsmDialect,
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue},
};

use crate::{
    ast::{Expression, expressions::FunctionCallExpr},
    intermediate::{
        arch::compile_arch_size_type, builtin::BuiltinFunction,
        compile_expr::compile_expression_to_value, compiler::CompilationContext,
        pointer::SmartValue,
    },
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct AsmBuiltin;

impl BuiltinFunction for AsmBuiltin {
    fn handle_call<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        expr: &FunctionCallExpr,
        compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<SmartValue<'ctx>> {
        let (asm_str, constraints) = match (&expr.arguments[0], &expr.arguments[1]) {
            (Expression::String(asm), Expression::String(cons)) => (asm, cons),
            _ => bail!("First two arguments must be string literals"),
        };

        let mut operands: Vec<BasicMetadataValueEnum> = Vec::new();
        let mut metadata_types: Vec<BasicMetadataTypeEnum> = Vec::new();

        for arg in &expr.arguments[2..] {
            let val =
                compile_expression_to_value(context, module, builder, arg, compilation_context)?;
            let val = val.unwrap(builder)?;
            metadata_types.push(val.get_type().into());
            operands.push(val.into());
        }

        let ret_void = !constraints.value.contains('=');
        let fn_type = if ret_void {
            context.void_type().fn_type(&metadata_types, false)
        } else {
            compile_arch_size_type(context).fn_type(&metadata_types, false)
        };

        let inline_asm = context.create_inline_asm(
            fn_type,
            asm_str.value.clone(),
            constraints.value.clone(),
            true,
            false,
            Some(InlineAsmDialect::Intel),
            false,
        );

        let call_site_value = builder.build_indirect_call(fn_type, inline_asm, &operands, "asm")?;

        Ok(if ret_void {
            SmartValue::from_value(context.i32_type().const_zero().as_basic_value_enum())
        } else {
            SmartValue::from_value(
                call_site_value
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("Espected call site value to be a basic value"))?,
            )
        })
    }
}
