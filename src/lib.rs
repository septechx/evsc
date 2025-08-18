mod ast;
mod backend;
mod bindings;
mod intermediate;
mod lexer;
mod parser;

use std::path::PathBuf;

use anyhow::Result;

use crate::{
    backend::BackendOptions, intermediate::CompileOptions as IRCompilerOptions,
    lexer::lexer::tokenize,
};

/// Options for the compiler
pub struct CompilerOptions {
    pub module_name: String,
    pub source_dir: PathBuf,
    pub output_file: PathBuf,
    pub emit_llvm: bool,
}

/// Compiles the given source code into an object file
pub fn compile(source: String, opts: &CompilerOptions) -> Result<Vec<u8>> {
    let tokens = tokenize(source)?;
    let ast = parser::parser::parse(tokens)?;

    let compiler_opts = IRCompilerOptions {
        module_name: &opts.module_name,
        source_dir: &opts.source_dir,
        output_file: &opts.output_file,
        emit_llvm: opts.emit_llvm,
        backend_options: &BackendOptions::default(),
    };

    intermediate::compile(ast, &compiler_opts)?;

    // TODO: Actually compile the IR

    Ok(Vec::new())
}
