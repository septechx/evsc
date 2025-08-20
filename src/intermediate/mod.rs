mod builtin;
mod compile_expr;
mod compile_type;
mod compiler;
mod emmiter;
mod import;
mod pointer;
mod resolve_lib;

use std::path::Path;

use anyhow::{anyhow, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    llvm_sys::{
        core::{LLVMConstArray2, LLVMSetInitializer},
        prelude::LLVMValueRef,
    },
    module::{Linkage, Module},
    types::AsTypeRef,
    values::{AsValueRef, BasicValue, BasicValueEnum, FunctionValue},
    AddressSpace, InlineAsmDialect,
};

use crate::{
    ast::statements::BlockStmt,
    backend::{
        build_assembly_file, build_executable, build_object_file, BackendOptions, LinkerKind,
    },
    errors::{CodeLine, CodeType, CompilationError, ErrorLevel, InfoBlock, SourceLocation},
    intermediate::{compiler::CompilationContext, emmiter::emit_to_file},
    ERRORS,
};

#[derive(Debug)]
pub struct CompileOptions<'a> {
    pub module_name: &'a str,
    pub source_dir: &'a Path,
    pub output_file: &'a Path,
    pub source_file: &'a Path,
    pub emit: &'a EmitType,
    pub backend_options: &'a BackendOptions,
    pub pic: bool,
    pub linker_kind: Option<LinkerKind>,
}

#[derive(Debug, PartialEq)]
pub enum EmitType {
    LLVM,
    Assembly,
    Object,
    Executable,
}

pub fn compile(ast: BlockStmt, opts: &CompileOptions) -> Result<()> {
    let context = Context::create();
    let module = context.create_module(opts.module_name);
    let builder = context.create_builder();

    let mut cc = CompilationContext::new(opts.source_dir.join(opts.module_name));
    inject_builtins(&context, &mut cc);

    let init_fn = setup_module(&context, &module, &builder)?;
    compiler::compile(&context, &module, &builder, &ast, &mut cc)?;
    emit_global_ctors(&context, &module, &builder, init_fn)?;

    if *opts.emit == EmitType::Executable {
        generate_c_runtime_integration(&context, &module, &builder, opts.source_file)?;
    }

    match opts.emit {
        EmitType::LLVM => emit_to_file(opts.output_file, &module)?,
        EmitType::Assembly => build_assembly_file(opts.output_file, &module, opts.backend_options)?,
        EmitType::Object => build_object_file(opts.output_file, &module, opts.backend_options)?,
        EmitType::Executable => {
            let linker_kind = if let Some(linker_kind) = opts.linker_kind {
                linker_kind
            } else {
                ERRORS.lock().add(CompilationError::new(
                    ErrorLevel::Fatal,
                    "Linker kind not specified for executable".to_string(),
                ));

                unreachable!()
            };

            let temp_obj_path = opts.output_file.with_extension("o");
            build_object_file(&temp_obj_path, &module, opts.backend_options)?;

            let object_files = vec![temp_obj_path.as_path()];
            build_executable(
                &object_files,
                opts.output_file,
                false,
                opts.pic,
                linker_kind,
            )?;

            if let Err(e) = std::fs::remove_file(&temp_obj_path) {
                ERRORS.lock().add(CompilationError::new(
                    ErrorLevel::Warning,
                    format!("Failed to remove temporary object file: {e}"),
                ));
            }
        }
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
            .left()
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

fn create_exit_syscall<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    exit_code: BasicValueEnum<'ctx>,
) -> Result<()> {
    let exit_ty = context
        .void_type()
        .fn_type(&[context.i32_type().into()], false);

    let exit_asm = context.create_inline_asm(
        exit_ty,
        "mov rax, 60\nmov rdi, $0\nsyscall".to_string(),
        "r".to_string(),
        true,
        false,
        Some(InlineAsmDialect::Intel),
        false,
    );

    builder.build_indirect_call(exit_ty, exit_asm, &[exit_code.into()], "exit_call")?;
    builder.build_return(None)?;

    Ok(())
}
