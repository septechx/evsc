mod ast;
mod lexer;
mod parser;

use lexer::lexer::tokenize;
use parser::parser::parse;
use std::fs;

fn main() {
    let files = fs::read_dir("examples").unwrap();
    let mut files: Vec<_> = files.collect();
    files.sort_by(|a, b| {
        let a = a.as_ref().unwrap().path();
        let b = b.as_ref().unwrap().path();
        a.file_name().unwrap().cmp(b.file_name().unwrap())
    });

    for file in files {
        let file = file.unwrap();
        let file_path = file.path();
        let file_path_str = file_path.to_string_lossy().into_owned();
        let file = fs::read_to_string(file_path).unwrap();
        let tokens = tokenize(file);
        let ast = parse(tokens);
        print!("=== {} ===\n{:#?}\n", file_path_str, ast);
    }
}
