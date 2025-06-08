mod lexer;

use lexer::lexer::Lexer;
use std::fs;

fn main() {
    let file = fs::read_to_string("examples/all.evsc").unwrap();

    let mut lexer = Lexer::new(file);
    let tokens = lexer.tokenize();

    println!("{:?}", tokens);
}
