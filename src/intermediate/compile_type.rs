use std::collections::HashMap;

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType},
};

use crate::{
    ast::ast::Type,
    intermediate::{
        builtin::slice::create_slice_struct,
        compiler::{StructDef, TypeContext},
    },
};

pub fn compile_type<'ctx>(
    context: &'ctx Context,
    ty: &Type,
    type_context: &mut TypeContext<'ctx>,
) -> BasicTypeEnum<'ctx> {
    match ty {
        // Skip const as LLVM does not support it
        Type::Const(inner) => compile_type(context, &inner.underlying, type_context),
        Type::Symbol(sym) => match sym.name.as_str() {
            "i32" => context.i32_type().as_basic_type_enum(),
            "u8" => context.i8_type().as_basic_type_enum(),
            "usize" => context.i64_type().as_basic_type_enum(),
            tyname => unimplemented!("{tyname}"),
        },
        Type::Slice(slice_ty) => {
            let element_ty = compile_type(context, &slice_ty.underlying, type_context);
            let name = format!(
                "Slice_{}",
                match &*slice_ty.underlying {
                    Type::Symbol(s) => &s.name,
                    _ => "unknown",
                }
            );

            if let Some(def) = type_context.struct_defs.get(&name) {
                return def.llvm_type.as_basic_type_enum();
            }

            let slice_struct = create_slice_struct(context, element_ty, &name);

            type_context.struct_defs.insert(
                name.clone(),
                StructDef {
                    llvm_type: slice_struct,
                    field_indices: HashMap::from([("ptr".to_string(), 0), ("len".to_string(), 1)]),
                },
            );

            type_context.struct_defs[&name]
                .llvm_type
                .as_basic_type_enum()
        }
        ty => unimplemented!("{ty:#?}"),
    }
}

pub fn compile_function_type<'ctx>(
    context: &'ctx Context,
    return_type: &Type,
    param_types: &[Type],
    type_context: &mut TypeContext<'ctx>,
) -> FunctionType<'ctx> {
    let return_llvm_type: Option<BasicTypeEnum> = match return_type {
        Type::Symbol(sym) if sym.name == "void" => None,
        _ => Some(compile_type(context, return_type, type_context)),
    };

    let param_llvm_types: Vec<_> = param_types
        .iter()
        .map(|ty| compile_type(context, ty, type_context).into())
        .collect();

    if let Some(ret_ty) = return_llvm_type {
        ret_ty.fn_type(&param_llvm_types, false)
    } else {
        context.void_type().fn_type(&param_llvm_types, false)
    }
}
