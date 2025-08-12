mod builtin;
mod compile_expr;
mod compile_type;
mod compiler;
mod emmiter;
mod import;
mod pointer;

use std::{env, fs};

use anyhow::Result;
use inkwell::{
    context::Context,
    llvm_sys::{
        core::{LLVMConstArray2, LLVMSetInitializer},
        prelude::LLVMValueRef,
    },
    module::Linkage,
    types::AsTypeRef,
    values::{AsValueRef, BasicValue},
    AddressSpace,
};

use crate::{ast::statements::BlockStmt, intermediate::compiler::CompilationContext};

pub fn compile(module_name: &str, ast: BlockStmt, path: &str) -> Result<()> {
    let context = Context::create();
    let module = context.create_module(module_name);
    let builder = context.create_builder();

    let absolute_path = fs::canonicalize(env::current_dir()?.join(path).join(module_name))?;

    let mut compilation_context = CompilationContext::new(absolute_path);

    builtin::create_slice_struct(&context, &mut compilation_context);

    let init_fn_type = context.void_type().fn_type(&[], false);
    let init_fn = module.add_function("__module_init", init_fn_type, None);
    let init_bb = context.append_basic_block(init_fn, "entry");
    builder.position_at_end(init_bb);

    compiler::compile(&context, &module, &builder, &ast, &mut compilation_context)?;

    if init_fn
        .get_last_basic_block()
        .unwrap()
        .get_terminator()
        .is_none()
    {
        builder.position_at_end(init_fn.get_last_basic_block().unwrap());
        builder.build_return(None)?;
    }

    let priority = context.i32_type().const_int(65535, false);

    let ctor_entry_ty = context.struct_type(
        &[
            context.i32_type().into(),
            context.ptr_type(AddressSpace::default()).into(),
            context.ptr_type(AddressSpace::default()).into(),
        ],
        false,
    );

    let ctor_entry_const = ctor_entry_ty.const_named_struct(&[
        priority.as_basic_value_enum(),
        init_fn
            .as_global_value()
            .as_pointer_value()
            .as_basic_value_enum(),
        context
            .ptr_type(AddressSpace::default())
            .const_null()
            .as_basic_value_enum(),
    ]);

    let ctors_array_ty = ctor_entry_ty.array_type(1);

    let entry_ref: LLVMValueRef = ctor_entry_const.as_value_ref();
    let elem_type_ref = ctor_entry_ty.as_type_ref();
    let array_ref = unsafe {
        LLVMConstArray2(
            elem_type_ref,
            &entry_ref as *const LLVMValueRef as *mut LLVMValueRef,
            1,
        )
    };

    let gv = module.add_global(ctors_array_ty, None, "llvm.global_ctors");
    unsafe {
        LLVMSetInitializer(gv.as_value_ref(), array_ref);
    }
    gv.set_linkage(Linkage::Appending);

    let output_name = module_name.strip_suffix(".evsc").unwrap_or(module_name);
    emmiter::emit_to_file(&format!("{path}/{output_name}.ll"), &module)?;

    Ok(())
}
