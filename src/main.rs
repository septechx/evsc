mod ast;
mod intermediate;
mod lexer;
mod parser;

use lexer::lexer::tokenize;
use parser::parser::parse;
use std::fs;

fn main() -> anyhow::Result<()> {
    let file = fs::read_to_string("_test/01.evsc")?;
    let tokens = tokenize(file)?;
    let ast = parse(tokens)?;
    intermediate::compile("01.evsc", ast)?;

    Ok(())
}
