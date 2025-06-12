mod ast;
mod lexer;
mod parser;

use lexer::lexer::tokenize;
use parser::parser::parse;
use std::fs;

fn main() {
    let file = fs::read_to_string("examples/02.evsc").unwrap();

    let tokens = tokenize(file);
    let ast = parse(tokens);

    println!("{:#?}", ast);
}
