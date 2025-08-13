use std::{fs, path::Path};

use crate::{
    intermediate::{self, CompileOptions},
    lexer::lexer::tokenize,
    parser::parser::parse,
};

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
                let file = fs::read_to_string(path)?;
                let tokens = tokenize(file)?;
                let ast = parse(tokens)?;
                let name_no_ext = name.strip_suffix(".evsc").unwrap();
                let opts = CompileOptions {
                    module_name: name,
                    source_dir: test_path,
                    output_file: &test_path.join(format!("{name}.ll")),
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
