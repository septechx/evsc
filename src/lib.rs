mod ast;
mod backend;
mod bindings;
mod intermediate;
mod lexer;
mod parser;

use std::path::PathBuf;

use anyhow::Result;

use crate::{
    backend::BackendOptions,
    intermediate::{CompileOptions as IRCompilerOptions, EmitType},
    lexer::lexer::tokenize,
};

/// Options for the compiler
pub struct CompilerOptions {
    pub module_name: String,
    pub source_dir: PathBuf,
    pub output_file: PathBuf,
    pub emit_llvm: bool,
    pub emit: EmitType,
}

/// Compiles the given source code into an object file
pub fn compile(source: String, opts: &CompilerOptions) -> Result<Vec<u8>> {
    // TODO: Implement

    Ok(Vec::new())
}
