use anyhow::{Result, anyhow};
use clang::{Clang, EntityKind, Index, source::SourceLocation};
use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    ast::{
        Ast, NodeId, Stmt, StmtKind, Type,
        statements::{FnArgument, FnDeclStmt},
        types::SymbolType,
    },
    codegen::{
        builtin::import::create_module,
        compiler::{self, CompilationContext},
        pointer::SmartValue,
    },
    span::{ModuleId, Span},
};
use std::fs;

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

    let mut ast: Vec<Stmt> = Vec::new();

    for (i, e) in tu.get_entity().get_children().iter().enumerate() {
        if e.get_kind() == EntityKind::FunctionDecl {
            let name = parse_function_name(e.get_display_name().expect("function has no name"))
                .expect("function has no name");
            let ty = parse_function_type(e.get_type().expect("function has no type"))?;

            let arguments =
                ty.1.into_iter()
                    .enumerate()
                    .map(|(i, arg)| FnArgument {
                        name: format!("arg{}", i),
                        type_: arg,
                    })
                    .collect::<Vec<_>>();

            let location = e.get_location().expect("function has no location");
            let (span, _module_id) = convert_clang_location(location);

            let stmt = Stmt {
                kind: StmtKind::FnDecl(FnDeclStmt {
                    name,
                    arguments,
                    body: Vec::new(),
                    return_type: ty.0,
                    is_public: true,
                    is_extern: true,
                }),
                id: NodeId(i),
                span,
                attributes: Vec::new(),
            };

            ast.push(stmt);
        }
    }

    let ast = Ast(ast);

    let mut mod_compilation_context = CompilationContext::new(module_path);
    compiler::compile_stmts(
        context,
        module,
        builder,
        &ast.0,
        &mut mod_compilation_context,
    )?;

    create_module(
        context,
        module_name,
        compilation_context,
        mod_compilation_context,
    )
}

fn parse_function_name(name: String) -> Option<String> {
    name.split_once('(').map(|(name, _)| name.to_string())
}

fn parse_function_type(ty: clang::Type) -> Result<(Type, Vec<Type>)> {
    let return_type = ty.get_result_type().expect("function type is not valid");
    let args = ty.get_argument_types().unwrap_or_default();

    let args: Vec<String> = args.iter().map(|arg| arg.get_display_name()).collect();
    let arg_types: Vec<Type> = args.iter().map(|arg| parse_type(arg)).collect();

    let return_type = parse_type(&return_type.get_display_name());

    Ok((return_type, arg_types))
}

fn parse_type(ty: &str) -> Type {
    let ty = map_c_type(ty);

    Type::Symbol(SymbolType {
        name: ty.to_string(),
    })
}

fn map_c_type(ty: &str) -> &str {
    match ty {
        "void" => "void",
        "bool" => "bool",
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

fn convert_clang_location(location: SourceLocation) -> (Span, ModuleId) {
    let loc = location.get_file_location();
    let clang_file = loc.file.expect("file location has no file").get_path();
    let file_path = clang_file.clone();

    let module_id = crate::SOURCE_MAPS.with(|sm| {
        let mut maps = sm.borrow_mut();
        if let Ok(content) = fs::read_to_string(&file_path) {
            maps.add_source(content, file_path)
        } else {
            ModuleId(0)
        }
    });

    (Span::new(loc.offset, loc.offset + 1), module_id)
}
