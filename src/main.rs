pub mod ast;
pub mod backend;
pub mod bindings;
pub mod cli;
pub mod errors;
pub mod intermediate;
pub mod lexer;
pub mod macros;
pub mod parser;
pub mod typecheck;

use std::{
    cell::RefCell,
    env, fs,
    path::{Path, PathBuf},
};

use clap::Parser;
use colored::Colorize;
use inkwell::targets::{CodeModel, RelocMode};

use crate::{
    backend::{BackendOptions, LinkerKind},
    cli::{Cli, OptLevel},
    errors::ErrorCollector,
    intermediate::{CompileOptions, EmitType},
    lexer::tokenize,
    parser::parse,
    typecheck::TypeChecker,
};

pub struct ErrorState {
    pub collector: RefCell<ErrorCollector>,
}

thread_local! {
    pub static ERRORS: ErrorState = ErrorState {
        collector: RefCell::new(ErrorCollector::new()),
    };
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let file_path = cli
        .files
        .iter()
        .find(|file| file.extension().unwrap() == "evsc")
        .ok_or(anyhow::anyhow!("No evsc files specified".red().bold()))?
        .clone(); // $1/$2.evsc
    build_file(file_path, &cli)?;

    ERRORS.with(|e| {
        e.collector.borrow().print_all();
    });

    Ok(())
}

fn check_for_errors() {
    if ERRORS.with(|e| e.collector.borrow().has_errors()) {
        ERRORS.with(|e| {
            e.collector.borrow().print_all();
        });
        std::process::exit(1);
    }
}

fn build_file(file_path: PathBuf, cli: &Cli) -> anyhow::Result<()> {
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
                } else if cli.files.len() > 1 {
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

    let reloc_mode = if cli.no_pie {
        RelocMode::Default
    } else if cli.shared {
        RelocMode::DynamicNoPic
    } else if cli.static_ {
        RelocMode::Static
    } else {
        RelocMode::PIC
    };

    let linker_kind = if cli.use_gcc_linker {
        LinkerKind::Gcc
    } else {
        LinkerKind::Ld
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
        emit: &emit,
        output_file: &output,
        source_file: &file_path,
        backend_options: &backend_opts,
        pic: !cli.no_pie,
        linker_kind: Some(linker_kind),
        cache_dir: None,
    };

    let source_text = fs::read_to_string(&file_path)?;

    let tokens = tokenize(source_text.clone(), &file_path)?;
    check_for_errors();

    let ast = parse(tokens.clone())?;
    check_for_errors();

    let typechecker = TypeChecker::new(file_path.clone(), tokens);
    typechecker.check(&ast.body)?;
    check_for_errors();

    intermediate::compile(ast, &opts)?;
    check_for_errors();

    if cli.files.len() > 1 {
        let additional_objects: Vec<&Path> = cli.files[1..]
            .iter()
            .filter(|f| f.extension().is_some_and(|ext| ext == "o"))
            .map(|f| f.as_path())
            .collect();

        if !additional_objects.is_empty() {
            let temp_obj_path = output.with_extension("o");
            let mut all_objects = vec![temp_obj_path.as_path()];
            all_objects.extend(additional_objects);
            backend::build_executable(
                &all_objects,
                &output.with_extension(""),
                cli.shared,
                !cli.no_pie,
                linker_kind,
            )?;
        }
    }

    Ok(())
}
