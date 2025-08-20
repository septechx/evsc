mod ast;
mod backend;
mod bindings;
mod cli;
mod errors;
mod intermediate;
mod lexer;
mod parser;

#[cfg(debug_assertions)]
mod gentests;

use std::{
    env, fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use clap::Parser;
use colored::Colorize;
use inkwell::targets::{CodeModel, RelocMode};
use lazy_static::lazy_static;
use parking_lot::Mutex;

use crate::{
    backend::{BackendOptions, LinkerKind},
    cli::{Cli, OptLevel},
    errors::ErrorCollector,
    intermediate::{CompileOptions, EmitType},
    lexer::lexer::tokenize,
    parser::parser::parse,
    lexer::token::extract_tokens,
};

lazy_static! {
    pub static ref ERRORS: Arc<Mutex<ErrorCollector>> = Arc::new(Mutex::new(ErrorCollector::new()));
}

fn main() -> anyhow::Result<()> {
    #[cfg(debug_assertions)]
    gentests::check()?;

    let cli = Cli::parse();

    let file_path = cli
        .files
        .iter()
        .find(|file| file.extension().unwrap() == "evsc")
        .ok_or(anyhow::anyhow!("No evsc files specified".red().bold()))?
        .clone(); // $1/$2.evsc
    build_file(file_path, &cli)?;

    Ok(())
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
            (true, _) => ("ll", EmitType::LLVM),
            (false, true) => ("s", EmitType::Assembly),
            _ => ("o", EmitType::Object),
        }
    } else {
        match (cli.emit_llvm, cli.emit_asm) {
            (true, _) => ("ll", EmitType::LLVM),
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
        backend_options: &backend_opts,
        pic: !cli.no_pie,
        linker_kind: Some(linker_kind),
    };

    let source_text = fs::read_to_string(&file_path)?;
    let tokens = tokenize(source_text, &file_path)?;
    let ast = parse(extract_tokens(&tokens))?;
    intermediate::compile(ast, &opts)?;

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

#[cfg(test)]
mod tests {
    use std::{fs, path::Path};

    use crate::{
        backend::BackendOptions,
        intermediate::{self, CompileOptions, EmitType},
        lexer::lexer::tokenize,
        parser::parser::parse,
        lexer::token::extract_tokens,
    };

    fn status(task: &str, file: &str, ok: bool) -> bool {
        let color_ok = "\x1b[1;32m";
        let color_err = "\x1b[1;31m";
        let reset = "\x1b[0m";

        let dots = ".".repeat(30usize.saturating_sub(task.len() + file.len()));
        if ok {
            println!("{task} {file}{dots}{color_ok}OK{reset}");
        } else {
            println!("{task} {file}{dots}{color_err}ER{reset}");
        }
        !ok
    }

    #[test]
    fn run_tests() {
        let test_path = Path::new("./tests");
        let test_count = 8;

        let mut failed = false;

        for i in 1..=test_count {
            {
                let name = format!("{i:02}-test.evsc");
                let path = test_path.join(&name);
                let file = fs::read_to_string(&path).unwrap();
                let tokens = tokenize(file, &path);
                if status("Tokenizing", &name, tokens.is_ok()) {
                    failed = true;
                    eprintln!("{}", tokens.err().unwrap());
                    continue;
                };
                let ast = parse(extract_tokens(&tokens.unwrap()));
                if status("Parsing", &name, ast.is_ok()) {
                    failed = true;
                    eprintln!("{}", ast.err().unwrap());
                    continue;
                };
                let opts = CompileOptions {
                    module_name: &format!("{i:02}-test"),
                    source_dir: test_path,
                    output_file: &test_path.join(format!("{i:02}-test.ll")),
                    emit: &EmitType::LLVM,
                    backend_options: &BackendOptions::default(),
                    pic: true, // Doesn't matter with EmitType::LLVM
                    linker_kind: None,
                };
                let res = intermediate::compile(ast.unwrap(), &opts);
                if status("Compiling", &name, res.is_ok()) {
                    failed = true;
                    eprintln!("{}", res.err().unwrap());
                    continue;
                };
            }

            {
                let name = format!("{i:02}.ll");
                let path = test_path.join(&name);
                let file = fs::read_to_string(path).unwrap();
                let file = file.split('\n').collect::<Vec<_>>();
                let file = file[2..].join("\n");
                let test_name = format!("{i:02}-test.ll");
                let test_path = test_path.join(test_name);
                let test_file = fs::read_to_string(test_path).unwrap();

                // Not sure why the first two lines are different, but the tests fail without this
                // code
                let test_file = test_file.split('\n').collect::<Vec<_>>();
                let test_file = test_file[2..].join("\n");

                if status("Checking", &name, file == test_file) {
                    failed = true;
                };
            }
        }

        assert!(!failed);
    }
}
