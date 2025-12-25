use anyhow::Result;
use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType},
    AddressSpace,
};

use crate::{
    ast::ast::Type,
    intermediate::{
        arch::compile_arch_size_type,
        builtin::{get_builtin, Builtin},
        compiler::CompilationContext,
    },
};

pub fn compile_type<'ctx>(
    context: &'ctx Context,
    ty: &Type,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<BasicTypeEnum<'ctx>> {
    Ok(match ty {
        // Skip const as LLVM does not support it
        Type::Const(inner) => compile_type(context, &inner.underlying, compilation_context)?,
        Type::Symbol(sym) => {
            match sym.name.as_str() {
                "u8" | "i8" => context.i8_type().as_basic_type_enum(),
                "u16" | "i16" => context.i16_type().as_basic_type_enum(),
                "u32" | "i32" => context.i32_type().as_basic_type_enum(),
                "u64" | "i64" => context.i64_type().as_basic_type_enum(),
                "u128" | "i128" => context.i128_type().as_basic_type_enum(),
                "f16" => context.f16_type().as_basic_type_enum(),
                "f32" => context.f32_type().as_basic_type_enum(),
                "f64" => context.f64_type().as_basic_type_enum(),
                "f128" => context.f128_type().as_basic_type_enum(),
                "usize" | "isize" => compile_arch_size_type(context).as_basic_type_enum(),
                // Any should map to *void
                "any" => context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
                tyname => unimplemented!("{tyname}"),
            }
        }
        Type::Slice(_) => get_builtin(context, compilation_context, Builtin::Slice)?
            .llvm_type
            .as_basic_type_enum(),
        Type::Function(_func_ty) => context
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
        ty => unimplemented!("{ty:#?}"),
    })
}

pub fn compile_function_type<'ctx>(
    context: &'ctx Context,
    return_type: &Type,
    param_types: &[Type],
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<FunctionType<'ctx>> {
    let return_llvm_type: Option<BasicTypeEnum> = match return_type {
        Type::Symbol(sym) if sym.name == "void" => None,
        _ => Some(compile_type(context, return_type, compilation_context)?),
    };

    let param_llvm_types: Vec<_> = param_types
        .iter()
        .map(|ty| compile_type(context, ty, compilation_context))
        .collect::<Result<Vec<_>>>()?
        .iter()
        .map(|&ty| ty.into())
        .collect();

    Ok(if let Some(ret_ty) = return_llvm_type {
        ret_ty.fn_type(&param_llvm_types, false)
    } else {
        context.void_type().fn_type(&param_llvm_types, false)
    })
}
