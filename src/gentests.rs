use std::{fs, path::Path};

use crate::{
    backend::BackendOptions,
    intermediate::{self, CompileOptions, EmitType},
    lexer::lexer::tokenize,
    parser::parser::parse,
    lexer::token::extract_tokens,
};

pub fn check() -> anyhow::Result<()> {
    if std::env::var("IS_DEV").is_ok() {
        let args: Vec<String> = std::env::args().collect();

        if args.len() == 2 && args[1] == "gen_tests" {
            clean_tests()?;
            println!("Cleaned tests");
            gen_tests()?;
            println!("Generated tests");
        }

        panic!("Done");
    }

    Ok(())
}

pub fn clean_tests() -> anyhow::Result<()> {
    let test_path = "tests";

    for entry in fs::read_dir(test_path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            let path2 = path.clone();
            let name = path2.file_name().unwrap().to_str().unwrap();
            if name.ends_with(".ll") {
                fs::remove_file(path)?;
            }
        }
    }

    Ok(())
}

pub fn gen_tests() -> anyhow::Result<()> {
    let test_path = Path::new("./tests");

    for entry in fs::read_dir(test_path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            let path2 = path.clone();
            let name = path2.file_name().unwrap().to_str().unwrap();
            if name.ends_with(".evsc") {
                let file = fs::read_to_string(&path)?;
                let tokens = tokenize(file, &path)?;
                let ast = parse(extract_tokens(&tokens))?;
                let name_no_ext = name.strip_suffix(".evsc").unwrap();
                let opts = CompileOptions {
                    module_name: name,
                    source_dir: test_path,
                    output_file: &test_path.join(format!("{name}.ll")),
                    emit: &EmitType::LLVM,
                    backend_options: &BackendOptions::default(),
                    pic: true, // Doesn't matter with EmitType::LLVM
                    linker_kind: None,
                };
                intermediate::compile(ast, &opts)?;
                let name_old = format!("{name}.ll");
                let name = name_no_ext.strip_suffix("-test").unwrap();
                let name = format!("{name}.ll");
                let path = test_path.join(name);
                let path_old = test_path.join(name_old);
                fs::rename(path_old, path)?;
            }
        }
    }

    Ok(())
}
