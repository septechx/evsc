pub mod linker;

use anyhow::{Result, anyhow};
use inkwell::{
    OptimizationLevel,
    llvm_sys::target_machine::LLVMGetDefaultTargetTriple,
    module::Module,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
};
use std::{ffi::CStr, path::Path};

use crate::backend::linker::linkers::{GccLinker, LdLinker};

#[derive(Debug)]
pub struct BackendOptions {
    pub opt_level: OptimizationLevel,
    pub reloc_mode: RelocMode,
    pub code_model: CodeModel,
    pub cpu: String,
    pub features: String,
}

impl Default for BackendOptions {
    fn default() -> Self {
        Self {
            opt_level: OptimizationLevel::Aggressive,
            reloc_mode: RelocMode::PIC,
            code_model: CodeModel::Default,
            cpu: "x86-64".to_string(),
            features: "+avx2".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinkerKind {
    Gcc,
    Ld,
}

pub fn prepare_module(module: &Module, options: &BackendOptions) -> Result<TargetMachine> {
    Target::initialize_all(&InitializationConfig::default());

    let target_triple = unsafe { CStr::from_ptr(LLVMGetDefaultTargetTriple()).to_str()? };
    let target_triple = TargetTriple::create(target_triple);
    let target = Target::from_triple(&target_triple).map_err(|e| anyhow!("{e}"))?;

    let target_machine = target
        .create_target_machine(
            &target_triple,
            &options.cpu,
            &options.features,
            options.opt_level,
            options.reloc_mode,
            options.code_model,
        )
        .ok_or(anyhow!("Failed to create target machine"))?;

    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&target_triple);

    Ok(target_machine)
}

pub fn build_object_file(
    output_path: &Path,
    module: &Module,
    options: &BackendOptions,
) -> Result<()> {
    let target_machine = prepare_module(module, options)?;

    target_machine
        .write_to_file(module, FileType::Object, output_path)
        .map_err(|e| anyhow!("{e}"))?;

    Ok(())
}

pub fn build_assembly_file(
    output_path: &Path,
    module: &Module,
    options: &BackendOptions,
) -> Result<()> {
    let target_machine = prepare_module(module, options)?;

    target_machine
        .write_to_file(module, FileType::Assembly, output_path)
        .map_err(|e| anyhow!("{e}"))?;

    Ok(())
}

pub fn build_executable(
    object_files: &[&Path],
    output_path: &Path,
    is_shared: bool,
    pie: bool,
    linker_kind: LinkerKind,
) -> Result<()> {
    match linker_kind {
        LinkerKind::Ld => {
            linker::link_object_files::<LdLinker>(object_files, output_path, is_shared, pie)
        }
        LinkerKind::Gcc => {
            linker::link_object_files::<GccLinker>(object_files, output_path, is_shared, pie)
        }
    }
}
