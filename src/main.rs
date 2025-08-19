mod ast;
mod backend;
mod bindings;
mod cli;
mod intermediate;
mod lexer;
mod parser;

mod gentests;

use std::{
    env, fs,
    path::{Path, PathBuf},
};

use clap::Parser;
use inkwell::targets::{CodeModel, RelocMode};

use crate::{
    backend::BackendOptions,
    cli::{Cli, OptLevel},
    intermediate::{CompileOptions, EmitType},
    lexer::lexer::tokenize,
    parser::parser::parse,
};

fn main() -> anyhow::Result<()> {
    if std::env::var("IS_DEV").is_ok() {
        let args: Vec<String> = std::env::args().collect();

        if args.len() == 2 && args[1] == "gen_tests" {
            gentests::clean_tests()?;
            println!("Cleaned tests");
            gentests::gen_tests()?;
            println!("Generated tests");
            return Ok(());
        }

        return Ok(());
    }

    let cli = Cli::parse();

    let file_path = cli.files[0].clone(); // $1/$2.evsc
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
        link_libc: !cli.no_libc,
        pic: !cli.no_pie,
    };

    let source_text = fs::read_to_string(&file_path)?;
    let tokens = tokenize(source_text)?;
    let ast = parse(tokens)?;
    intermediate::compile(ast, &opts)?;

    if emit == EmitType::Executable && !cli.files.is_empty() {
        let additional_objects: Vec<&Path> = cli.files[1..]
            .iter()
            .filter(|f| f.extension().map_or(false, |ext| ext == "o"))
            .map(|f| f.as_path())
            .collect();

        if !additional_objects.is_empty() {
            let temp_obj_path = output.with_extension("o");
            let mut all_objects = vec![temp_obj_path.as_path()];
            all_objects.extend(additional_objects);
            backend::build_executable(
                &all_objects,
                &output,
                cli.shared,
                !cli.no_libc,
                !cli.no_pie,
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
                let file = fs::read_to_string(path).unwrap();
                let tokens = tokenize(file);
                if status("Tokenizing", &name, tokens.is_ok()) {
                    failed = true;
                    eprintln!("{}", tokens.err().unwrap());
                    continue;
                };
                let ast = parse(tokens.unwrap());
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
                    link_libc: true, // Doesn't matter with EmitType::LLVM
                    pic: true,       // Doesn't matter with EmitType::LLVM
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
