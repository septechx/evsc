mod ast;
mod backend;
mod bindings;
mod cli;
mod intermediate;
mod lexer;
mod parser;

mod gentests;

use std::fs;

use clap::Parser;
use inkwell::targets::{CodeModel, RelocMode};

use crate::{
    backend::BackendOptions,
    cli::{Cli, OptLevel},
    intermediate::CompileOptions,
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

    // TODO: Handle multiple files and link them together

    let file = cli.files[0].clone(); // $1/$2.evsc
    let path = file.parent().unwrap(); // $1/
    let name = file.file_name().unwrap().to_str().unwrap(); // $2

    let extension = if cli.emit_llvm { "ll" } else { "o" };
    let output = cli.output.unwrap_or(file.with_extension(extension)); // $3 or $1/$2.ll|o

    let cpu = cli.cpu.unwrap_or("x86-64".to_string());
    let features = cli.features.unwrap_or("+avx2".to_string());
    let opt = cli.opt.unwrap_or(OptLevel::O3);

    let reloc_mode = if cli.pic {
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
        module_name: name,
        source_dir: path,
        output_file: &output,
        emit_llvm: cli.emit_llvm,
        backend_options: &backend_opts,
    };

    let file = fs::read_to_string(&file)?;
    let tokens = tokenize(file)?;
    let ast = parse(tokens)?;
    intermediate::compile(ast, &opts)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{fs, path::Path};

    use crate::{
        backend::BackendOptions,
        intermediate::{self, CompileOptions},
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
                    emit_llvm: true,
                    backend_options: &BackendOptions::default(),
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
