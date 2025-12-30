use std::{collections::HashMap, fs, iter::Extend};

use anyhow::{Result, bail};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum},
};

use crate::{
    ast::{ExprKind, expressions::FunctionCallExpr},
    bindings::llvm_bindings::create_named_struct,
    codegen::{
        builtin::{
            BuiltinFunction,
            import::{header::compile_header, resolve_lib::resolve_std_lib},
        },
        compiler::{self, CompilationContext, StructDef},
        pointer::SmartValue,
    },
    errors::builders,
    lexer::tokenize,
    parser::parse,
};

mod header;
mod resolve_lib;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct ImportBuiltin;

impl BuiltinFunction for ImportBuiltin {
    fn handle_call<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        expr: &FunctionCallExpr,
        compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<SmartValue<'ctx>> {
        // Resolve path
        if expr.arguments.len() != 1 {
            bail!("Expected one argument to @import");
        }

        let module_name = match &expr.arguments[0].kind {
            ExprKind::String(sym) => sym.value.clone(),
            _ => bail!("Expected string literal as argument to @import"),
        };

        match ModuleType::from(&module_name) {
            ModuleType::Library | ModuleType::Evsc => {
                compile_evsc_module(context, module, builder, module_name, compilation_context)
            }
            ModuleType::Header => {
                compile_header(context, module, builder, module_name, compilation_context)
            }
        }
        // Resolve path
    }
}

fn compile_evsc_module<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    module_name: String,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    // Resolve path
    let module_path = if module_name == "std" {
        resolve_std_lib()?
    } else {
        compilation_context
            .module_path
            .parent()
            .unwrap()
            .join(&module_name)
    };
    // Resolve path

    // Compile module
    let file = fs::read_to_string(&module_path);
    if let Err(err) = file {
        crate::ERRORS.with(|e| {
            e.borrow_mut().add(builders::fatal(format!(
                "Module `{}` (resolved to {}) not found: {}",
                module_name,
                module_path.display(),
                err
            )));
        });
        unreachable!();
    }
    let file = file.unwrap();

    let (tokens, _module_id) = tokenize(file, &module_path)?;
    let ast = parse(tokens)?;

    let mut mod_compilation_context = CompilationContext::new(module_path);

    compiler::compile_stmts(
        context,
        module,
        builder,
        &ast.0,
        &mut mod_compilation_context,
    )?;
    // Compile module

    create_module(
        context,
        module_name,
        compilation_context,
        mod_compilation_context,
    )
}

pub fn create_module<'ctx, 'mctx>(
    context: &'ctx Context,
    module_name: String,
    compilation_context: &mut CompilationContext<'ctx>,
    mod_compilation_context: CompilationContext<'mctx>,
) -> Result<SmartValue<'ctx>>
where
    'mctx: 'ctx,
{
    // Transfer builtins
    compilation_context
        .builtins
        .extend(mod_compilation_context.builtins);

    compilation_context.type_context.struct_defs.extend(
        mod_compilation_context
            .type_context
            .struct_defs
            .into_iter()
            .filter(|(_, def)| def.is_builtin)
            .collect::<HashMap<_, _>>(),
    );
    // Transfer builtins

    // Create module struct
    let entries: Vec<(String, BasicTypeEnum)> = mod_compilation_context
        .symbol_table
        .iter()
        .map(|(name, entry)| (name.clone(), entry.value.value.get_type()))
        .collect();

    let module_name = format!(
        "Module_{}",
        module_name.strip_suffix(".evsc").unwrap_or(&module_name)
    );

    let values: Vec<BasicTypeEnum> = entries.iter().map(|(_, ty)| *ty).collect();

    let struct_ty = create_named_struct(context, &values, &module_name, false)?;

    let mut field_indices: HashMap<String, u32> = HashMap::new();
    for (i, (name, _ty)) in entries.iter().enumerate() {
        field_indices.insert(name.clone(), i as u32);
    }

    compilation_context.type_context.struct_defs.insert(
        module_name.clone(),
        StructDef {
            llvm_type: struct_ty,
            field_indices,
            is_builtin: false,
        },
    );

    let mut const_fields: Vec<BasicValueEnum> = Vec::new();
    for (name, _) in entries.iter() {
        let entry = mod_compilation_context
            .symbol_table
            .get(name)
            .expect("entry existed when we collected entries");
        const_fields.push(entry.value.value);
    }

    let const_struct = struct_ty.const_named_struct(&const_fields);
    // Create module struct

    Ok(SmartValue::from_value(const_struct.as_basic_value_enum()))
}

enum ModuleType {
    Library,
    Evsc,
    Header,
}

impl From<&String> for ModuleType {
    fn from(path: &String) -> Self {
        if path.ends_with(".evsc") {
            ModuleType::Evsc
        } else if path.ends_with(".h") {
            ModuleType::Header
        } else {
            ModuleType::Library
        }
    }
}
