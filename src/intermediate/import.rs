use std::{collections::HashMap, fs, path::PathBuf};

use anyhow::{bail, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, BasicTypeEnum},
    values::BasicValue,
};

use crate::{
    ast::{ast::Expression, expressions::FunctionCallExpr},
    bindings::llvm_bindings::create_named_struct,
    intermediate::{
        compiler::{self, CompilationContext},
        pointer::SmartValue,
        resolve_lib::resolve_std_lib,
    },
    lexer::lexer::tokenize,
    parser::parser::parse,
};

pub fn import_module<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    expr: &FunctionCallExpr,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    if expr.arguments.len() != 1 {
        bail!("Expected one argument to @import");
    }

    let module_name = match &expr.arguments[0] {
        Expression::String(sym) => sym.value.clone(),
        _ => bail!("Expected string literal as argument to @import"),
    };

    let module_path = if module_name == "std" {
        resolve_std_lib()?
    } else {
        PathBuf::from(&module_name)
    };

    let module_path = compilation_context
        .module_path
        .parent()
        .unwrap()
        .join(module_path);

    let file = fs::read_to_string(&module_path)?;

    let tokens = tokenize(file)?;
    let ast = parse(tokens)?;

    let mut mod_compilation_context = CompilationContext::new(module_path);

    // Copy builtins to not have duplicate ones when merging the symbol tables
    // TODO: Find a better way to do this
    for (name, def) in &compilation_context.type_context.struct_defs {
        if name == "Slice" {
            mod_compilation_context
                .type_context
                .struct_defs
                .insert(name.clone(), def.clone());
        }
    }

    compiler::compile(context, module, builder, &ast, &mut mod_compilation_context)?;

    let mut entries: Vec<(String, BasicTypeEnum)> = mod_compilation_context
        .symbol_table
        .iter()
        .map(|(name, entry)| (name.clone(), entry.value.value.get_type()))
        .collect();

    entries.sort_by(|a, b| a.0.cmp(&b.0));

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
        crate::intermediate::compiler::StructDef {
            llvm_type: struct_ty,
            field_indices,
        },
    );

    let mut const_fields: Vec<inkwell::values::BasicValueEnum> = Vec::new();
    for (name, _) in entries.iter() {
        let entry = mod_compilation_context
            .symbol_table
            .get(name)
            .expect("entry existed when we collected entries");
        const_fields.push(entry.value.value);
    }

    let const_struct = context.const_struct(&const_fields, false);

    let gv = module.add_global(struct_ty, None, &format!("inst_{module_name}"));
    gv.set_initializer(&const_struct);
    gv.set_linkage(Linkage::Private);
    gv.set_constant(true);

    let gv_ptr = gv.as_pointer_value().as_basic_value_enum();
    Ok(SmartValue::from_pointer(
        gv_ptr,
        struct_ty.as_basic_type_enum(),
    ))
}
