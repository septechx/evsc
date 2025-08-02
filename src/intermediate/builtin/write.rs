use anyhow::{bail, Result};
use inkwell::values::BasicValue;
use inkwell::AddressSpace;
use inkwell::{builder::Builder, context::Context, module::Module, values::BasicValueEnum};

use crate::ast::expressions::FunctionCallExpr;
use crate::intermediate::compile_expr::compile_expression_to_value;

pub fn handle_write_call<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    expr: &FunctionCallExpr,
) -> Result<BasicValueEnum<'ctx>> {
    if expr.arguments.len() != 3 {
        bail!("write requires exactly 3 arguments");
    }

    let fd_val = compile_expression_to_value(context, module, builder, &expr.arguments[0])?;
    let buf_val = compile_expression_to_value(context, module, builder, &expr.arguments[1])?;
    let len_val = compile_expression_to_value(context, module, builder, &expr.arguments[2])?;

    let fd_ext =
        builder.build_int_z_extend(fd_val.into_int_value(), context.i64_type(), "fd_ext")?;
    let len_ext =
        builder.build_int_z_extend(len_val.into_int_value(), context.i64_type(), "len_ext")?;

    let fn_ty = context.void_type().fn_type(
        &[
            context.i64_type().into(),
            context.ptr_type(AddressSpace::default()).into(),
            context.i64_type().into(),
        ],
        false,
    );

    let asm_str = "
        mov rax, 1
        mov rdi, $0
        mov rsi, $1
        mov rdx, $2
        syscall
    ";

    let constraint_str = "r,r,r,~{rax},~{rdi},~{rsi},~{rdx},~{rcx},~{r11},~{cc}";

    let inline = context.create_inline_asm(
        fn_ty,
        asm_str.to_string(),
        constraint_str.to_string(),
        true,
        false,
        Some(inkwell::InlineAsmDialect::Intel),
        false,
    );

    builder.build_indirect_call(
        fn_ty,
        inline,
        &[fd_ext.into(), buf_val.into(), len_ext.into()],
        "write_syscall",
    )?;

    Ok(context.i32_type().const_int(0, false).as_basic_value_enum())
}
