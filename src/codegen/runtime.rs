use anyhow::{Result, anyhow};
use inkwell::{
    InlineAsmDialect,
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, BasicValueEnum},
};

use crate::{
    codegen::arch::{compile_arch_size_type, is_64},
    errors::widgets::CodeType,
    fatal, warning_with_example,
};

pub fn generate_c_runtime_integration<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
) -> Result<()> {
    let start_fn_type = context.void_type().fn_type(&[], false);
    let start_fn = module.add_function("_start", start_fn_type, None);

    let entry_bb = context.append_basic_block(start_fn, "entry");
    builder.position_at_end(entry_bb);

    let init_fn = module
        .get_function("__module_init")
        .expect("module init function not found");
    builder.build_call(init_fn, &[], "init_call")?;

    if let Some(main_fn) = module.get_function("main") {
        let main_ty = main_fn.get_type();
        let ret_ty = main_ty.get_return_type();

        let main_result = builder.build_call(main_fn, &[], "main_call")?;

        // main() returns void => None
        let result_value = if let Some(ret_ty) = ret_ty {
            // TODO: Move this check to the type checker
            if !ret_ty.is_int_type() {
                fatal!("Main function must return an integer or void");
            }

            main_result
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| anyhow!("Expected main to return a basic value"))?
        } else {
            compile_arch_size_type(context)
                .const_zero()
                .as_basic_value_enum()
        };

        create_exit_syscall(context, builder, result_value)?;
    } else {
        warning_with_example!(
            "Main function not found",
            "pub fn main() void {}",
            1,
            CodeType::Add
        );

        let exit_code = compile_arch_size_type(context)
            .const_zero()
            .as_basic_value_enum();
        create_exit_syscall(context, builder, exit_code)?;
    }

    Ok(())
}

fn create_exit_syscall<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    exit_code: BasicValueEnum<'ctx>,
) -> Result<()> {
    let (asm, exit_ty, target_int_ty) = if is_64() {
        let exit_ty = context
            .void_type()
            .fn_type(&[context.i64_type().into()], false);
        let asm = "mov rdi, $0\nmov rax, 60\nsyscall".to_string();
        (asm, exit_ty, context.i64_type())
    } else {
        let exit_ty = context
            .void_type()
            .fn_type(&[context.i32_type().into()], false);
        let asm = "mov ebx, $0\nmov eax, 60\nint 0x80".to_string();
        (asm, exit_ty, context.i32_type())
    };

    let exit_code_int = exit_code.into_int_value();
    let extended_exit_code =
        if exit_code_int.get_type().get_bit_width() < target_int_ty.get_bit_width() {
            builder.build_int_z_extend(exit_code_int, target_int_ty, "extended_exit_code")?
        } else if exit_code_int.get_type().get_bit_width() > target_int_ty.get_bit_width() {
            builder.build_int_truncate(exit_code_int, target_int_ty, "truncated_exit_code")?
        } else {
            exit_code_int
        };

    let inline = context.create_inline_asm(
        exit_ty,
        asm,
        "r,~{rax},~{rdi},~{rcx},~{r11},~{memory}".to_string(),
        true,
        false,
        Some(InlineAsmDialect::Intel),
        false,
    );
    builder.build_indirect_call(exit_ty, inline, &[extended_exit_code.into()], "exit_call")?;

    builder.build_unreachable()?;

    Ok(())
}
