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
    io::IsTerminal,
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
            Linker,
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

pub fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.quiet {
        ENABLE_PRINTING.with(|e| *e.borrow_mut() = false);
    }

    if cli.input.len() > 1 {
        eprintln!("{}", "Multiple input files specified".red().bold());
        std::process::exit(1);
    }

    let file_path = cli.input.first().unwrap().clone();

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
        let use_color = match cli.color {
            cli::ColorChoice::Always => true,
            cli::ColorChoice::Never => false,
            cli::ColorChoice::Auto => {
                std::io::stdout().is_terminal() && std::env::var("NO_COLOR").is_err()
            }
        };
        colored::control::set_override(use_color);
        logln!("{}", ast.display(use_color));
        return Ok(());
    }

    let source_dir = file_path
        .parent()
        .expect("source file must have a parent directory"); // $1/

    let module_name = file_path
        .file_stem()
        .and_then(|s| s.to_str())
        .expect("file name must be valid UTF-8"); // $2

    let (extension, emit) = 'block: {
        if cli.emit_llvm {
            break 'block ("ll", EmitType::Llvm);
        }
        if cli.emit_asm {
            break 'block ("s", EmitType::Assembly);
        }
        if cli.shared {
            break 'block ("so", EmitType::Object);
        }
        if cli.no_link {
            break 'block ("o", EmitType::Object);
        }
        break 'block ("", EmitType::Executable);
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

    Ok(())
}
