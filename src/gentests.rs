use std::fs;

use crate::{intermediate, lexer::lexer::tokenize, parser::parser::parse};

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
    let test_path = "tests";

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
                intermediate::compile(name, ast, test_path)?;
                let name = name.strip_suffix(".evsc").unwrap();
                let name_old = format!("{name}.ll");
                let name = name.strip_suffix("-test").unwrap();
                let name = format!("{name}.ll");
                let path = format!("{test_path}/{name}");
                let path_old = format!("{test_path}/{name_old}");
                fs::rename(path_old, path)?;
            }
        }
    }

    Ok(())
}
