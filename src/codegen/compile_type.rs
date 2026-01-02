use anyhow::Result;
use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValue, BasicValueEnum},
};

use crate::{
    ast::{Type, TypeKind},
    codegen::{
        arch::compile_arch_size_type,
        builtin::{Builtin, get_builtin},
        compiler::CompilationContext,
    },
};

pub fn cast_int_to_type<'ctx>(
    builder: &Builder<'ctx>,
    val: BasicValueEnum<'ctx>,
    dest_type: BasicTypeEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>> {
    if val.get_type() != dest_type && val.is_int_value() && dest_type.is_int_type() {
        let src_int = val.into_int_value();
        let dest_int_type = dest_type.into_int_type();
        let src_width = src_int.get_type().get_bit_width();
        let dest_width = dest_int_type.get_bit_width();

        if src_width > dest_width {
            return Ok(builder
                .build_int_truncate(src_int, dest_int_type, "trunc")?
                .as_basic_value_enum());
        } else if src_width < dest_width {
            return Ok(builder
                .build_int_s_extend(src_int, dest_int_type, "sext")?
                .as_basic_value_enum());
        }
    }
    Ok(val)
}

pub fn compile_type<'ctx>(
    context: &'ctx Context,
    ty: &Type,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<BasicTypeEnum<'ctx>> {
    Ok(match &ty.kind {
        // Skip mut as LLVM does not support it
        TypeKind::Mut(inner) => compile_type(context, &inner.underlying, compilation_context)?,
        TypeKind::Symbol(sym) => match sym.name.value.as_ref() {
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
            tyname => unimplemented!("{tyname}"),
        },
        TypeKind::Slice(_) => get_builtin(context, compilation_context, Builtin::Slice)?
            .llvm_type
            .as_basic_type_enum(),
        TypeKind::Function(_) | TypeKind::Pointer(_) => context
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
    let return_llvm_type: Option<BasicTypeEnum> = match &return_type.kind {
        TypeKind::Symbol(sym) if sym.name.value.as_ref() == "void" => None,
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
