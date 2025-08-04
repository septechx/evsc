use std::collections::HashMap;

use inkwell::{
    context::Context,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    AddressSpace,
};

use crate::{
    ast::ast::Type,
    intermediate::{
        builtin::slice::create_slice_struct,
        compiler::{CompilationContext, StructDef},
    },
};

pub fn compile_type<'ctx>(
    context: &'ctx Context,
    ty: &Type,
    compilation_context: &mut CompilationContext<'ctx>,
) -> BasicTypeEnum<'ctx> {
    match ty {
        // Skip const as LLVM does not support it
        Type::Const(inner) => compile_type(context, &inner.underlying, compilation_context),
        Type::Symbol(sym) => match sym.name.as_str() {
            "i32" => context.i32_type().as_basic_type_enum(),
            "u8" => context.i8_type().as_basic_type_enum(),
            "usize" => context.i64_type().as_basic_type_enum(),
            tyname => unimplemented!("{tyname}"),
        },
        Type::Slice(_) => {
            if let Some(def) = compilation_context.type_context.struct_defs.get("Slice") {
                return def.llvm_type.as_basic_type_enum();
            }

            let slice_struct = create_slice_struct(context);

            compilation_context.type_context.struct_defs.insert(
                "Slice".to_string(),
                StructDef {
                    llvm_type: slice_struct,
                    field_indices: HashMap::from([("ptr".to_string(), 0), ("len".to_string(), 1)]),
                },
            );

            compilation_context
                .type_context
                .struct_defs
                .get("Slice")
                .unwrap()
                .llvm_type
                .as_basic_type_enum()
        }
        Type::Function(func_ty) => {
            let param_types: Vec<BasicTypeEnum> = func_ty
                .parameters
                .iter()
                .map(|param| compile_type(context, param, compilation_context))
                .collect();

            let param_types: Vec<BasicMetadataTypeEnum> =
                param_types.iter().map(|&t| t.into()).collect();

            match &*func_ty.return_type {
                Type::Symbol(sym) if sym.name == "void" => {
                    let return_type = context.void_type();
                    return_type
                        .fn_type(&param_types, false)
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum()
                }
                _ => {
                    let return_type =
                        compile_type(context, &func_ty.return_type, compilation_context);

                    return_type
                        .fn_type(&param_types, false)
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum()
                }
            }
        }
        ty => unimplemented!("{ty:#?}"),
    }
}

pub fn compile_function_type<'ctx>(
    context: &'ctx Context,
    return_type: &Type,
    param_types: &[Type],
    compilation_context: &mut CompilationContext<'ctx>,
) -> FunctionType<'ctx> {
    let return_llvm_type: Option<BasicTypeEnum> = match return_type {
        Type::Symbol(sym) if sym.name == "void" => None,
        _ => Some(compile_type(context, return_type, compilation_context)),
    };

    let param_llvm_types: Vec<_> = param_types
        .iter()
        .map(|ty| compile_type(context, ty, compilation_context).into())
        .collect();

    if let Some(ret_ty) = return_llvm_type {
        ret_ty.fn_type(&param_llvm_types, false)
    } else {
        context.void_type().fn_type(&param_llvm_types, false)
    }
}
