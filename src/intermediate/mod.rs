mod builtin;
mod compile_expr;
mod compile_type;
mod compiler;
mod emmiter;
mod import;
mod pointer;
mod resolve_lib;

use std::path::Path;

use anyhow::Result;
use inkwell::{
    builder::Builder,
    context::Context,
    llvm_sys::{
        core::{LLVMConstArray2, LLVMSetInitializer},
        prelude::LLVMValueRef,
    },
    module::{Linkage, Module},
    types::AsTypeRef,
    values::{AsValueRef, BasicValue, FunctionValue},
    AddressSpace,
};

use crate::{
    ast::statements::BlockStmt,
    backend::{build_assembly_file, build_object_file, BackendOptions},
    intermediate::{compiler::CompilationContext, emmiter::emit_to_file},
};

#[derive(Debug)]
pub struct CompileOptions<'a> {
    pub module_name: &'a str,
    pub source_dir: &'a Path,
    pub output_file: &'a Path,
    pub emit: &'a EmitType,
    pub backend_options: &'a BackendOptions,
}

#[derive(Debug)]
pub enum EmitType {
    LLVM,
    Assembly,
    Object,
}

pub fn compile(ast: BlockStmt, opts: &CompileOptions) -> Result<()> {
    let context = Context::create();
    let module = context.create_module(opts.module_name);
    let builder = context.create_builder();

    let mut cc = CompilationContext::new(opts.source_dir.join(opts.module_name));
    inject_builtins(&context, &mut cc);

    let init_fn = setup_module(&context, &module, &builder);
    compiler::compile(&context, &module, &builder, &ast, &mut cc)?;
    emit_global_ctors(&context, &module, &builder, init_fn)?;

    match opts.emit {
        EmitType::LLVM => emit_to_file(opts.output_file, &module)?,
        EmitType::Assembly => build_assembly_file(opts.output_file, &module, opts.backend_options)?,
        EmitType::Object => build_object_file(opts.output_file, &module, opts.backend_options)?,
    }

    Ok(())
}

fn inject_builtins<'ctx>(context: &'ctx Context, cc: &mut CompilationContext<'ctx>) {
    builtin::create_slice_struct(context, cc);
}

fn setup_module<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
) -> FunctionValue<'ctx> {
    let init_fn_type = context.void_type().fn_type(&[], false);
    let init_fn = module.add_function("__module_init", init_fn_type, None);
    let init_bb = context.append_basic_block(init_fn, "entry");
    builder.position_at_end(init_bb);

    init_fn
}

fn emit_global_ctors<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    init_fn: FunctionValue<'ctx>,
) -> Result<()> {
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

    Ok(())
}
