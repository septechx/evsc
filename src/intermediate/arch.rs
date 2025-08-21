use anyhow::Result;
use inkwell::{builder::Builder, context::Context, values::BasicValueEnum, InlineAsmDialect};

use crate::{
    errors::{CompilationError, ErrorLevel},
    ERRORS,
};

pub fn is_64() -> bool {
    match std::env::consts::ARCH {
        "x86" | "arm" | "m68k" | "mips" | "mips32r6" | "csky" | "powerpc" | "riscv32" | "s390x"
        | "sparc" | "hexagon" => false,
        "x86_64" | "aarch64" | "mips64" | "mips64r6" | "powerpc64" | "riscv64" | "sparc64"
        | "loongarch64" => true,
        _ => {
            ERRORS.lock().add(CompilationError::new(
                ErrorLevel::Fatal,
                format!("Unknown architecture `{}`", std::env::consts::ARCH),
            ));
            unreachable!()
        }
    }
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
        let asm = "mov rax, 60\nmov rdi, $0\nsyscall".to_string();
        (asm, exit_ty)
    } else {
        let exit_ty = context
            .void_type()
            .fn_type(&[context.i32_type().into()], false);
        let asm = "mov eax, 60\nmov ebx, $0\nint 0x80".to_string();
        (asm, exit_ty)
    };

    let inline = context.create_inline_asm(
        exit_ty,
        asm,
        "r".to_string(),
        true,
        false,
        Some(InlineAsmDialect::Intel),
        false,
    );
    builder.build_indirect_call(exit_ty, inline, &[exit_code.into()], "exit_call")?;

    builder.build_return(None)?;

    Ok(())
}
