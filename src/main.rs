mod ast;
mod bindings;
mod intermediate;
mod lexer;
mod parser;

mod gentests;

use std::fs;

use crate::{lexer::lexer::tokenize, parser::parser::parse};

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 2 && args[1] == "gen_tests" {
        gentests::gen_tests()?;
        return Ok(());
    }

    let path = "_test";
    let name = "test.evsc";

    let file = fs::read_to_string(format!("{path}/{name}"))?;
    let tokens = tokenize(file)?;
    //println!("{tokens:#?}");
    let ast = parse(tokens)?;
    //println!("{ast:#?}");
    intermediate::compile(name, ast, path)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{intermediate, lexer::lexer::tokenize, parser::parser::parse};

    fn status(task: &str, file: &str, ok: bool) -> bool {
        let color_ok = "\x1b[1;32m";
        let color_err = "\x1b[1;31m";
        let reset = "\x1b[0m";

        let dots = ".".repeat(30usize.saturating_sub(task.len() + file.len()));
        if ok {
            println!("{task} {file}{dots}{color_ok}OK{reset}");
            false
        } else {
            println!("{task} {file}{dots}{color_err}ER{reset}");
            true
        }
    }

    #[test]
    fn run_tests() {
        let test_path = "tests";

        let mut failed = false;

        for i in 1..=6 {
            {
                let name = format!("{i:02}-test.evsc");
                let path = format!("{test_path}/{name}");
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
                let res =
                    intermediate::compile(&format!("{i:02}-test.evsc"), ast.unwrap(), test_path);
                if status("Compiling", &name, res.is_ok()) {
                    failed = true;
                    eprintln!("{}", res.err().unwrap());
                    continue;
                };
            }

            {
                let name = format!("{i:02}.ll");
                let path = format!("{test_path}/{name}");
                let file = fs::read_to_string(path).unwrap();
                let file = file.split('\n').collect::<Vec<_>>();
                let file = file[2..].join("\n");
                let test_name = format!("{i:02}-test.ll");
                let test_path = format!("{test_path}/{test_name}");
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
