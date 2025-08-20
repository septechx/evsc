use anyhow::Result;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, BasicValue},
    InlineAsmDialect,
};

use crate::{
    ast::{ast::Expression, expressions::FunctionCallExpr},
    errors::ErrorLevel,
    intermediate::{
        compile_expr::compile_expression_to_value,
        compiler::CompilationContext,
        pointer::{get_value, SmartValue},
    },
    ERRORS,
};

pub fn handle_asm_call<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    expr: &FunctionCallExpr,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    let (asm_str, constraints) = match (&expr.arguments[0], &expr.arguments[1]) {
        (Expression::String(asm), Expression::String(cons)) => (asm, cons),
        _ => {
            ERRORS.lock().add_simple(
                ErrorLevel::Error,
                "First two arguments must be string literals".to_string(),
            );
            return Ok(SmartValue::from_value(
                context.i32_type().const_int(0, false).as_basic_value_enum(),
            ));
        }
    };

    let mut operands: Vec<BasicMetadataValueEnum> = Vec::new();
    let mut metadata_types: Vec<BasicMetadataTypeEnum> = Vec::new();

    for arg in &expr.arguments[2..] {
        let val = compile_expression_to_value(context, module, builder, arg, compilation_context)?;
        let val = get_value(builder, &val)?;
        metadata_types.push(val.get_type().into());
        operands.push(val.into());
    }

    let fn_type = context.void_type().fn_type(&metadata_types, false);

    let inline_asm = context.create_inline_asm(
        fn_type,
        asm_str.value.clone(),
        constraints.value.clone(),
        true,
        false,
        Some(InlineAsmDialect::Intel),
        false,
    );

    builder.build_indirect_call(fn_type, inline_asm, &operands, "asm")?;

    Ok(SmartValue::from_value(
        context.i32_type().const_int(0, false).as_basic_value_enum(),
    ))
}
