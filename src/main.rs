pub mod ast;
pub mod backend;
pub mod bindings;
pub mod cli;
pub mod codegen;
pub mod errors;
pub mod lexer;
pub mod macros;
pub mod parser;
pub mod span;
pub mod utils;

use std::{
    cell::RefCell,
    env, fs,
    marker::PhantomData,
    path::{Path, PathBuf},
};

use anyhow::Result;
use clap::Parser;
use colored::Colorize;
use inkwell::targets::{CodeModel, RelocMode};

use crate::{
    backend::{
        BackendOptions,
        linker::{
            Linker, link_object_files,
            linkers::{GccLinker, LdLinker},
        },
    },
    cli::{Cli, OptLevel},
    codegen::{CompileOptions, EmitType},
    errors::{ErrorCollector, builders},
    lexer::tokenize,
    parser::parse,
    span::sourcemaps::SourceMapManager,
};

pub static DEFAULT_ROOT: &str = "..";

thread_local! {
    pub static ERRORS: RefCell<ErrorCollector> = RefCell::new(ErrorCollector::new());
    pub static SOURCE_MAPS: RefCell<SourceMapManager> = RefCell::new(SourceMapManager::default());
    pub static ENABLE_PRINTING: RefCell<bool> = const { RefCell::new(true) };
}

/// Program entry point for the compiler CLI.
///
/// Parses command-line arguments, selects the first input file with an `.oxi` extension,
/// configures printing according to the `quiet` flag, invokes the appropriate linker-specific
/// build flow, and prints any collected errors.
///
/// This function will print "No files specified" (in red and bold) and exit the process with
/// code 1 if no `.oxi` input file is present. If the `quiet` flag is set, it disables
/// runtime printing before building.
///
/// # Examples
///
/// ```no_run
/// // Typical invocation from a test or example harness:
/// use your_crate::main;
/// main().unwrap();
/// ```
///
/// # Returns
///
/// `Ok(())` on successful completion of parsing, building, and error reporting; an `Err`
/// if a failure occurs during build setup or compilation.
pub fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.quiet {
        ENABLE_PRINTING.with(|e| *e.borrow_mut() = false);
    }

    let file_path = cli.input.iter().find(|file| {
        if let Some(ext) = file.extension() {
            ext == "oxi"
        } else {
            false
        }
    }); // $1/$2.oxi

    if file_path.is_none() {
        eprintln!("{}", "No files specified".red().bold());
        std::process::exit(1);
    }

    let file_path = file_path.unwrap().clone();

    if cli.use_gcc {
        build_file::<GccLinker>(file_path, &cli)?;
    } else {
        build_file::<LdLinker>(file_path, &cli)?;
    }

    ERRORS.with(|e| {
        e.borrow().print_all();
    });

    Ok(())
}

fn check_for_errors() {
    if ERRORS.with(|e| e.borrow().has_errors()) {
        ERRORS.with(|e| {
            e.borrow().print_all();
        });
        std::process::exit(1);
    }
}

/// Compile a single source file according to CLI options, emit the selected artifact, and optionally link additional object files.
///
/// This function reads `file_path`, tokenizes and parses its contents, runs code generation with backend options derived from `cli`, and writes the resulting output artifact. If multiple input files are present on the CLI, additional `.o` files are linked into the final output when applicable. If the source file cannot be read, a fatal error is recorded in the global error collector.
///
/// # Returns
///
/// `Ok(())` on success; otherwise propagates an error produced during tokenization, parsing, code generation, or linking.
///
/// # Examples
///
/// ```ignore
/// use std::path::PathBuf;
/// // Assume `cli` is constructed from program arguments.
/// // build_file will compile `main.oxi` according to `cli`.
/// build_file::<LdLinker>(PathBuf::from("main.oxi"), &cli).unwrap();
/// ```
fn build_file<T: Linker>(file_path: PathBuf, cli: &Cli) -> Result<()> {
    let source_text = match fs::read_to_string(&file_path) {
        Err(err) => {
            ERRORS.with(|e| {
                e.borrow_mut().add(builders::fatal(format!(
                    "Source file `{}` not found: {}",
                    file_path.display(),
                    err
                )));
            });
            unreachable!();
        }
        Ok(source_text) => source_text,
    };

    let (tokens, module_id) = tokenize(source_text, &file_path)?;
    check_for_errors();

    let ast = parse(tokens)?;
    check_for_errors();

    if cli.print_ast {
        logln!("{:#?}", ast);
        return Ok(());
    }

    let source_dir = file_path
        .parent()
        .expect("source file must have a parent directory"); // $1/

    let module_name = file_path
        .file_stem()
        .and_then(|s| s.to_str())
        .expect("file name must be valid UTF-8"); // $2

    let (extension, emit) = if cli.no_link {
        match (cli.emit_llvm, cli.emit_asm) {
            (true, _) => ("ll", EmitType::Llvm),
            (false, true) => ("s", EmitType::Assembly),
            _ => ("o", EmitType::Object),
        }
    } else {
        match (cli.emit_llvm, cli.emit_asm) {
            (true, _) => ("ll", EmitType::Llvm),
            (false, true) => ("s", EmitType::Assembly),
            _ => {
                if cli.shared {
                    ("so", EmitType::Object)
                } else if cli.input.len() > 1 {
                    ("o", EmitType::Object)
                } else {
                    ("", EmitType::Executable)
                }
            }
        }
    };

    let output = cli.output.clone().unwrap_or_else(|| {
        if emit == EmitType::Executable {
            env::current_dir()
                .expect("failed to get current dir")
                .join(Path::new(module_name))
        } else {
            env::current_dir()
                .expect("failed to get current dir")
                .join(Path::new(module_name).with_extension(extension))
        }
    });

    let cpu = cli.cpu.clone().unwrap_or_else(|| "x86-64".to_string());
    let features = cli.features.clone().unwrap_or_else(|| "+avx2".to_string());
    let opt = cli.opt.unwrap_or(OptLevel::O3);

    let reloc_mode = if !cli.no_pic {
        RelocMode::PIC
    } else if cli.shared {
        RelocMode::DynamicNoPic
    } else if cli.static_ {
        RelocMode::Static
    } else {
        RelocMode::Default
    };

    let backend_opts = BackendOptions {
        code_model: CodeModel::Default,
        opt_level: opt.into(),
        reloc_mode,
        cpu,
        features,
    };

    let opts = CompileOptions {
        module_name,
        source_dir,
        module_id,
        emit: &emit,
        output_file: &output,
        source_file: &file_path,
        backend_options: &backend_opts,
        pie: !cli.no_pie && !cli.static_ && !cli.no_pic,
        static_linking: cli.static_,
        cache_dir: None,
        linker_kind: PhantomData::<T>,
    };

    codegen::compile(ast, &opts)?;
    check_for_errors();

    if cli.input.len() > 1 {
        let additional_objects: Vec<&Path> = cli.input[1..]
            .iter()
            .filter(|f| f.extension().is_some_and(|ext| ext == "o"))
            .map(|f| f.as_path())
            .collect();

        if !additional_objects.is_empty() {
            let temp_obj_path = output.with_extension("o");
            let mut all_objects = vec![temp_obj_path.as_path()];
            all_objects.extend(additional_objects);
            link_object_files::<T>(
                &all_objects,
                &output.with_extension(""),
                cli.shared,
                !cli.no_pie && !cli.static_ && !cli.no_pic,
                cli.static_,
            )?;
        }
    }

    Ok(())
}