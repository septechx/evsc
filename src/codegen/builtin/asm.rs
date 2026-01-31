use anyhow::{anyhow, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue},
    InlineAsmDialect,
};

use crate::{
    ast::{Expr, ExprKind, Literal},
    codegen::{
        arch::compile_arch_size_type, builtin::BuiltinFunction,
        compile_expr::compile_expression_to_value, compiler::CompilationContext,
        pointer::SmartValue,
    },
};

fn fail<'ctx>() -> Result<SmartValue<'ctx>> {
    Err(anyhow!(
        "Invalid parameters to asm builtin, expected @asm(string, string, tuple) (First 2 arguments must be available at compile time)",
    ))
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct AsmBuiltin;

impl BuiltinFunction for AsmBuiltin {
    fn handle_call<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
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

        if parameters.len() != 3 {
            return fail();
        }

        let (asm_str, constraints, elements) = match (
            &parameters[0].kind,
            &parameters[1].kind,
            &parameters[2].kind,
        ) {
            (
                ExprKind::Literal(asm),
                ExprKind::Literal(cons),
                ExprKind::TupleLiteral { elements },
            ) => match (asm, cons) {
                (Literal::String(asm), Literal::String(cons)) => (asm, cons, elements),
                _ => return fail(),
            },
            _ => {
                return fail();
            }
        };

        let mut operands: Vec<BasicMetadataValueEnum> = Vec::new();
        let mut metadata_types: Vec<BasicMetadataTypeEnum> = Vec::new();

        for arg in elements.iter() {
            let val =
                compile_expression_to_value(context, module, builder, arg, compilation_context)?;
            let val = val.unwrap(builder)?;
            metadata_types.push(val.get_type().into());
            operands.push(val.into());
        }

        let ret_void = !constraints.contains('=');
        let fn_type = if ret_void {
            context.void_type().fn_type(&metadata_types, false)
        } else {
            compile_arch_size_type(context).fn_type(&metadata_types, false)
        };

        let inline_asm = context.create_inline_asm(
            fn_type,
            asm_str.to_string(),
            constraints.to_string(),
            true,
            false,
            Some(InlineAsmDialect::Intel),
            false,
        );

        let call_site_value = builder.build_indirect_call(fn_type, inline_asm, &operands, "asm")?;

        Ok(if ret_void {
            SmartValue::from_value(
                context
                    .struct_type(&[], false)
                    .get_undef()
                    .as_basic_value_enum(),
            )
        } else {
            SmartValue::from_value(
                call_site_value
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("Expected call site value to be a basic value"))?,
            )
        })
    }
}
