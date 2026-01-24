use anyhow::{Result, anyhow};
use clang::{Clang, EntityKind, Index, source::SourceLocation};
use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    ast::{
        Ast, Ident, NodeId, Stmt, StmtKind, Type, TypeKind,
        statements::{FnArgument, FnDeclStmt},
        types::SymbolType,
    },
    codegen::{
        builtin::import::create_module,
        compiler::{self, CompilationContext},
        pointer::SmartValue,
    },
    span::{PackageId, Span},
};
use std::fs;

struct NodeIdManager {
    next_id: NodeId,
}

impl NodeIdManager {
    fn new() -> Self {
        Self { next_id: NodeId(0) }
    }

    fn next_id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id = NodeId(self.next_id.0 + 1);
        id
    }
}

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

    let mut nid_mgr = NodeIdManager::new();

    let mut ast: Vec<Stmt> = Vec::new();

    for (i, e) in tu.get_entity().get_children().iter().enumerate() {
        if e.get_kind() == EntityKind::FunctionDecl {
            let name = parse_function_name(e.get_display_name().expect("function has no name"))
                .expect("function has no name")
                .into();
            let ty =
                parse_function_type(e.get_type().expect("function has no type"), &mut nid_mgr)?;

            let arguments =
                ty.1.into_iter()
                    .enumerate()
                    .map(|(i, arg)| FnArgument {
                        name: Ident {
                            value: format!("arg{}", i).into(),
                            span: arg.span,
                        },
                        type_: arg,
                    })
                    .collect::<Box<[_]>>();

            let location = e.get_location().expect("function has no location");
            let (span, _module_id) = convert_clang_location(location);

            let stmt = Stmt {
                kind: StmtKind::FnDecl(FnDeclStmt {
                    name: Ident { value: name, span },
                    arguments,
                    body: None,
                    return_type: ty.0,
                    is_public: true,
                    is_extern: true,
                }),
                id: NodeId(i as u32),
                span,
                attributes: Box::new([]),
            };

            ast.push(stmt);
        }
    }

    let ast = Ast {
        name: module_name.clone().into_boxed_str(),
        items: ast.into_boxed_slice(),
    };

    let module_id = crate::SOURCE_MAPS.with(|sm| {
        let mut maps = sm.borrow_mut();
        maps.next_id()
    });

    let mut mod_compilation_context = CompilationContext::new(module_path, module_id);
    compiler::compile_stmts(
        context,
        module,
        builder,
        &ast.items,
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

fn parse_function_type(ty: clang::Type, nid_mgr: &mut NodeIdManager) -> Result<(Type, Vec<Type>)> {
    let return_type = ty.get_result_type().expect("function type is not valid");
    let args = ty.get_argument_types().unwrap_or_default();

    let arg_types: Vec<Type> = args
        .iter()
        .map(|arg| {
            let loc = arg
                .get_declaration()
                .expect("argument has no location")
                .get_location()
                .expect("argument has no location");
            let (span, _) = convert_clang_location(loc);
            let id = nid_mgr.next_id();
            parse_type(&arg.get_display_name(), id, span)
        })
        .collect();

    let loc = return_type
        .get_declaration()
        .expect("return type has no location")
        .get_location()
        .expect("return type has no location");
    let (span, _) = convert_clang_location(loc);
    let id = nid_mgr.next_id();
    let return_type = parse_type(&return_type.get_display_name(), id, span);

    Ok((return_type, arg_types))
}

fn parse_type(ty: &str, id: NodeId, span: Span) -> Type {
    let ty = map_c_type(ty);

    Type {
        kind: TypeKind::Symbol(SymbolType {
            name: Ident {
                value: ty.into(),
                span,
            },
        }),
        id,
        span,
    }
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

fn convert_clang_location(location: SourceLocation) -> (Span, PackageId) {
    let loc = location.get_file_location();
    let clang_file = loc.file.expect("file location has no file").get_path();
    let file_path = clang_file.clone();

    let module_id = crate::SOURCE_MAPS.with(|sm| {
        let mut maps = sm.borrow_mut();
        if let Ok(content) = fs::read_to_string(&file_path) {
            maps.add_source(content, file_path)
        } else {
            PackageId(0)
        }
    });

    (Span::new(loc.offset, loc.offset + 1), module_id)
}
