use std::path::Path;

use anyhow::{Result, anyhow};
use inkwell::{
    InlineAsmDialect, builder::Builder, context::Context, module::Module, values::BasicValueEnum,
};

use crate::{
    ERRORS,
    errors::{CodeLine, CodeType, CompilationError, ErrorLevel, InfoBlock, SourceLocation},
    intermediate::arch::is_64,
};

pub fn generate_c_runtime_integration<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    source_file: &Path,
) -> Result<()> {
    let start_fn_type = context.void_type().fn_type(&[], false);
    let start_fn = module.add_function("_start", start_fn_type, None);

    let entry_bb = context.append_basic_block(start_fn, "entry");
    builder.position_at_end(entry_bb);

    if let Some(init_fn) = module.get_function("__module_init") {
        builder.build_call(init_fn, &[], "init_call")?;
    }

    if let Some(main_fn) = module.get_function("main") {
        let main_result = builder.build_call(main_fn, &[], "main_call")?;
        let result_value = main_result
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| anyhow!("Expected main to return a basic value"))?;

        create_exit_syscall(context, builder, result_value)?;
    } else {
        ERRORS.lock().add(
            CompilationError::new(ErrorLevel::Fatal, "Main function not found".to_string())
                .with_code(CodeLine::new(
                    1,
                    "pub fn main() void {}".to_string(),
                    CodeType::Add,
                ))
                .with_info(InfoBlock::new(
                    "Add a main function or compile with `--no-link`".to_string(),
                ))
                .with_location(SourceLocation::new(source_file.to_path_buf(), 1, 1, 1)),
        );
    }

    builder.build_unreachable()?;

    Ok(())
}

pub fn create_exit_syscall<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    exit_code: BasicValueEnum<'ctx>,
) -> Result<()> {
    let (asm, exit_ty) = if is_64() {
        let exit_ty = context
            .void_type()
            .fn_type(&[context.i64_type().into()], false);
        let asm = "mov rdi, $0\nmov rax, 60\nsyscall".to_string();
        (asm, exit_ty)
    } else {
        let exit_ty = context
            .void_type()
            .fn_type(&[context.i32_type().into()], false);
        let asm = "mov ebx, $0\nmov eax, 60\nint 0x80".to_string();
        (asm, exit_ty)
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
    builder.build_indirect_call(exit_ty, inline, &[exit_code.into()], "exit_call")?;

    builder.build_unreachable()?;

    Ok(())
}
