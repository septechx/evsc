use std::{collections::HashMap, fs, iter::Extend, path::PathBuf};

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
        builtin::BuiltinFunction,
        compiler::{self, CompilationContext, StructDef},
        pointer::SmartValue,
        resolve_lib::resolve_std_lib,
    },
    lexer::lexer::tokenize,
    parser::parser::parse,
};

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
        // Resolve path

        // Compile module
        let file = fs::read_to_string(&module_path)?;

        let tokens = tokenize(file, &module_path)?;
        let ast = parse(tokens)?;

        let mut mod_compilation_context = CompilationContext::new(module_path);

        compiler::compile(context, module, builder, &ast, &mut mod_compilation_context)?;
        // Compile module

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
            crate::intermediate::compiler::StructDef {
                llvm_type: struct_ty,
                field_indices,
                is_builtin: false,
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
        // Create module struct

        // Instantiate module struct
        let gv = module.add_global(struct_ty, None, &format!("inst_{module_name}"));
        gv.set_initializer(&const_struct);
        gv.set_linkage(Linkage::Private);
        gv.set_constant(true);

        let gv_ptr = gv.as_pointer_value().as_basic_value_enum();
        Ok(SmartValue::from_pointer(
            gv_ptr,
            struct_ty.as_basic_type_enum(),
        ))
        // Instantiate module struct
    }
}
