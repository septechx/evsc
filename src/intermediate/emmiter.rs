use std::path::Path;

use anyhow::{anyhow, Result};
use inkwell::module::Module;

pub fn emit_to_file(file: &Path, module: &Module) -> Result<()> {
    module
        .print_to_file(file)
        .map_err(|llvm_err| anyhow!(llvm_err.to_string()))?;

    Ok(())
}
