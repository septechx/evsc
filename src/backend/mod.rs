use std::{ffi::CStr, path::Path};

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
            reloc_mode: RelocMode::Default,
            code_model: CodeModel::Default,
            cpu: "x86-64".to_string(),
            features: "+avx2".to_string(),
        }
    }
}

use anyhow::{anyhow, Result};
use inkwell::{
    llvm_sys::target_machine::LLVMGetDefaultTargetTriple,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    OptimizationLevel,
};

pub fn build_object_file(
    output_path: &Path,
    module: &Module,
    options: &BackendOptions,
) -> Result<()> {
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

    target_machine
        .write_to_file(module, FileType::Object, output_path)
        .map_err(|e| anyhow!("{e}"))?;

    Ok(())
}
