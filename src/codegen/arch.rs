use inkwell::{context::Context, types::IntType};

use crate::errors::builders;

pub fn is_64() -> bool {
    match std::env::consts::ARCH {
        "x86" | "arm" | "m68k" | "mips" | "mips32r6" | "csky" | "powerpc" | "riscv32" | "s390x"
        | "hexagon" => false,
        "x86_64" | "aarch64" | "mips64" | "mips64r6" | "powerpc64" | "riscv64" | "loongarch64" => {
            true
        }
        _ => {
            crate::ERRORS.with(|e| {
                e.collector.borrow_mut().add(builders::fatal(format!(
                    "Unknown architecture `{}`",
                    std::env::consts::ARCH
                )));
            });
            unreachable!()
        }
    }
}

pub fn compile_arch_size_type<'ctx>(context: &'ctx Context) -> IntType<'ctx> {
    if is_64() {
        context.i64_type()
    } else {
        context.i32_type()
    }
}
