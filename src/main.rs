mod ast;
mod intermediate;
mod lexer;
mod parser;

use lexer::lexer::tokenize;
use parser::parser::parse;
use std::fs;

fn main() -> anyhow::Result<()> {
    let file = fs::read_to_string("_test/test.evsc")?;
    let tokens = tokenize(file)?;
    //println!("{tokens:#?}");
    let ast = parse(tokens)?;
    //println!("{ast:#?}");
    intermediate::compile("test.evsc", ast)?;

    Ok(())
}
