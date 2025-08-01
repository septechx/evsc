use anyhow::{Result, anyhow};
use inkwell::module::Module;

pub fn emit_to_file(file_name: &str, module: &Module) -> Result<()> {
    module
        .print_to_file(file_name)
        .map_err(|llvm_err| anyhow!(llvm_err.to_string()))?;

    Ok(())
}
