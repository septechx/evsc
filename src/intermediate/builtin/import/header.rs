use std::path::Path;

use anyhow::{Result, anyhow};
use clang::{Clang, EntityKind, Index};
use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    ast::{
        Statement, Type,
        statements::{BlockStmt, FnArgument, FnDeclStmt},
    },
    intermediate::{
        builtin::import::create_module,
        compiler::{self, CompilationContext},
        pointer::SmartValue,
    },
    lexer::tokenize,
    parser::{Parser, types::parse_symbol_type},
};

pub fn compile_header<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    module_name: String,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    let module_path = compilation_context
        .module_path
        .parent()
        .unwrap()
        .join(module_name.clone());

    let clang = Clang::new().map_err(|err| anyhow!("clang: {err}"))?;
    let index = Index::new(&clang, false, false);
    let tu = index.parser(module_path.clone()).parse()?;

    let mut functions: Vec<FnDeclStmt> = Vec::new();

    for e in tu.get_entity().get_children() {
        if e.get_kind() == EntityKind::FunctionDecl {
            let name = parse_function_name(e.get_display_name().expect("function has no name"))
                .expect("function has no name");
            let ty = parse_function_type(
                e.get_type()
                    .expect("function has no type")
                    .get_display_name(),
                &e.get_location()
                    .expect("function has no location")
                    .get_file_location()
                    .file
                    .expect("function has no location")
                    .get_path(),
            )?;

            let arguments =
                ty.1.iter()
                    .enumerate()
                    .map(|(i, arg)| FnArgument {
                        name: format!("arg{}", i),
                        explicit_type: Some(arg.clone()),
                    })
                    .collect::<Vec<_>>();

            let stmt = FnDeclStmt {
                name,
                arguments,
                body: Vec::new(),
                explicit_type: ty.0,
                is_public: true,
                is_extern: true,
                attributes: Vec::new(),
            };

            functions.push(stmt);
        }
    }

    let ast = BlockStmt {
        body: functions.into_iter().map(Statement::FnDecl).collect(),
    };

    let mut mod_compilation_context = CompilationContext::new(module_path);
    compiler::compile(context, module, builder, &ast, &mut mod_compilation_context)?;

    create_module(
        context,
        module,
        module_name,
        compilation_context,
        mod_compilation_context,
    )
}

fn parse_function_name(name: String) -> Option<String> {
    name.split_once('(').map(|(name, _)| name.to_string())
}

fn parse_function_type(ty: String, file_path: &Path) -> Result<(Type, Vec<Type>)> {
    let (return_type, args) = ty.split_once(' ').expect("function type is not valid");
    let args = args
        .strip_prefix('(')
        .and_then(|args| args.strip_suffix(')'))
        .expect("function type is not valid");
    let args: Vec<&str> = args.split(',').map(|arg| arg.trim()).collect();

    let arg_types: Vec<Type> = args
        .iter()
        .map(|arg| parse_type(arg, file_path))
        .collect::<Result<Vec<Type>>>()?;

    let return_type = parse_type(return_type, file_path)?;

    Ok((return_type, arg_types))
}

fn parse_type(ty: &str, file_path: &Path) -> Result<Type> {
    let tokens = tokenize(map_c_type(ty).to_string(), file_path)?;
    let mut parser = Parser::new(tokens);
    parse_symbol_type(&mut parser)
}

fn map_c_type(ty: &str) -> &str {
    match ty {
        "char" => "i8",
        "short" => "i16",
        "int" => "i32",
        "long" => "i64",
        "long long" => "i128",
        "unsigned char" => "u8",
        "unsigned short" => "u16",
        "unsigned int" => "u32",
        "unsigned long" => "u64",
        "unsigned long long" => "u128",
        "float" => "f32",
        "double" => "f64",
        sym => sym,
    }
}
