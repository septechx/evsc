mod arch;
mod builtin;
mod compile_expr;
mod compile_type;
mod compiler;
mod emmiter;
mod inkwell_ext;
mod pointer;
mod runtime;

use std::{
    fs, io,
    marker::PhantomData,
    path::{Path, PathBuf},
};

use anyhow::{Result, anyhow, bail};
use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    llvm_sys::{
        core::{LLVMConstArray2, LLVMSetInitializer},
        prelude::LLVMValueRef,
    },
    module::{Linkage, Module},
    types::AsTypeRef,
    values::{AsValueRef, BasicValue, FunctionValue},
};

use crate::{
    ast::Ast,
    backend::{
        BackendOptions, build_assembly_file, build_object_file,
        linker::{Linker, link_object_files},
    },
    check_for_errors,
    codegen::{
        compiler::{CompilationContext, compile_stmts},
        emmiter::emit_to_file,
        runtime::generate_c_runtime_integration,
    },
    span::PackageId,
};

#[derive(Debug)]
pub struct CompileOptions<'a, T: Linker> {
    pub module_name: &'a str,
    pub source_dir: &'a Path,
    pub output_file: &'a Path,
    pub source_file: &'a Path,
    pub module_id: PackageId,
    pub emit: &'a EmitType,
    pub backend_options: &'a BackendOptions,
    pub pie: bool,
    pub static_linking: bool,
    pub cache_dir: Option<&'a Path>,
    pub linker_kind: PhantomData<T>,
}

#[derive(Debug, PartialEq)]
pub enum EmitType {
    Llvm,
    Assembly,
    Object,
    Executable,
}

pub fn compile<T: Linker>(ast: Ast, opts: &CompileOptions<T>) -> Result<()> {
    let context = Context::create();
    let module = context.create_module(opts.module_name);
    let builder = context.create_builder();

    let mut cc = CompilationContext::new(opts.source_dir.join(opts.module_name), opts.module_id);

    let init_fn = setup_module(&context, &module, &builder)?;
    compile_stmts(&context, &module, &builder, &ast.items, &mut cc)?;
    emit_global_ctors(&context, &module, &builder, init_fn)?;

    check_for_errors();

    if *opts.emit == EmitType::Executable {
        generate_c_runtime_integration(&context, &module, &builder)?;
    }

    module
        .verify()
        .map_err(|e| anyhow!("Module verification failed: {}", e))?;

    match opts.emit {
        EmitType::Llvm => emit_to_file(opts.output_file, &module)?,
        EmitType::Assembly => build_assembly_file(opts.output_file, &module, opts.backend_options)?,
        EmitType::Object => build_object_file(opts.output_file, &module, opts.backend_options)?,
        EmitType::Executable => {
            let tmp_dir = get_tmp_dir(opts.cache_dir)?;
            let obj_filename = opts
                .output_file
                .file_name()
                .ok_or_else(|| anyhow::anyhow!("Invalid output file path"))?
                .to_str()
                .ok_or_else(|| anyhow::anyhow!("Invalid UTF-8 in output file name"))?;
            let tmp_obj_path = tmp_dir.join("o").join(format!("{obj_filename}.o"));

            build_object_file(&tmp_obj_path, &module, opts.backend_options)?;

            let object_files = vec![tmp_obj_path.as_path()];
            link_object_files::<T>(
                &object_files,
                opts.output_file,
                false,
                opts.pie,
                opts.static_linking,
            )?;
        }
    }

    Ok(())
}

fn get_tmp_dir(custom_path: Option<&Path>) -> Result<PathBuf> {
    let dir = if let Some(p) = custom_path {
        p.to_path_buf()
    } else {
        PathBuf::from(std::env::var("OXI_CACHE_DIR").unwrap_or_else(|_| String::from(".oxi")))
    };

    let status = fs::create_dir_all(&dir).map_err(|e| match e.kind() {
        io::ErrorKind::AlreadyExists => anyhow::Ok(()),
        _ => bail!("Failed to create cache directory: {e}"),
    });

    if status.is_err() {
        bail!("Failed to create cache directory: {status:?}");
    }

    let obj_dir = dir.join("o");
    let status = fs::create_dir_all(&obj_dir).map_err(|e| match e.kind() {
        io::ErrorKind::AlreadyExists => anyhow::Ok(()),
        _ => bail!("Failed to create object cache directory: {e}"),
    });

    if status.is_err() {
        bail!("Failed to create object cache directory: {status:?}");
    }

    Ok(dir)
}

fn setup_module<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
) -> Result<FunctionValue<'ctx>> {
    let init_fn_type = context.void_type().fn_type(&[], false);
    let init_fn = module.add_function("__module_init", init_fn_type, None);
    let init_bb = context.append_basic_block(init_fn, "entry");
    builder.position_at_end(init_bb);

    Ok(init_fn)
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
