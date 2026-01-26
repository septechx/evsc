#![deny(clippy::unwrap_used)]

pub mod ast;
pub mod backend;
pub mod bindings;
pub mod cli;
pub mod codegen;
pub mod errors;
pub mod hashmap;
pub mod hir;
pub mod lexer;
pub mod macros;
pub mod parser;
pub mod span;
pub mod utils;

use std::{
    cell::{Cell, RefCell},
    fs,
    io::IsTerminal,
};

use anyhow::Result;
use clap::Parser;

use crate::{
    ast::validate::validate_ast, cli::Cli, errors::ErrorCollector, hir::lower_ast, lexer::tokenize,
    parser::parse, span::sourcemaps::SourceMapManager,
};

pub static DEFAULT_ROOT: &str = "..";

thread_local! {
    pub static ERRORS: RefCell<ErrorCollector> = RefCell::new(ErrorCollector::new());
    pub static SOURCE_MAPS: RefCell<SourceMapManager> = RefCell::new(SourceMapManager::default());
    pub static ENABLE_PRINTING: Cell<bool> = const { Cell::new(true) };
}

pub fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.quiet {
        ENABLE_PRINTING.with(|e| e.set(false));
    }

    build_file(cli)?;

    ERRORS.with(|e| {
        e.borrow().print_all();
    });

    Ok(())
}

fn check_for_errors() {
    if ERRORS.with(|e| e.borrow().has_errors()) {
        ERRORS.with(|e| {
            e.borrow().print_all();
        });
        std::process::exit(1);
    }
}

fn build_file(cli: Cli) -> Result<()> {
    let mut asts = Vec::with_capacity(cli.input.len());
    for file_path in cli.input {
        let source_text = match fs::read_to_string(&file_path) {
            Err(err) => fatal!(format!(
                "Source file `{}` not found: {}",
                file_path.display(),
                err
            )),
            Ok(source_text) => source_text,
        };

        let (tokens, module_id) = tokenize(source_text, &file_path)?;
        check_for_errors();

        let ast = parse(
            tokens,
            file_path
                .file_stem()
                .expect("file has stem")
                .to_str()
                .expect("name is valid UTF-8"),
        )?;
        check_for_errors();

        if cli.print_ast {
            let use_color = match cli.color {
                cli::ColorChoice::Always => true,
                cli::ColorChoice::Never => false,
                cli::ColorChoice::Auto => {
                    std::io::stdout().is_terminal() && std::env::var("NO_COLOR").is_err()
                }
            };
            colored::control::set_override(use_color);
            logln!("{}", ast.display(use_color)?);
        }

        validate_ast(&ast, module_id);
        check_for_errors();

        asts.push(ast);
    }

    if cli.print_ast {
        return Ok(());
    }

    let hir = lower_ast(asts);

    println!("{:#?}", hir);

    Ok(())
}
