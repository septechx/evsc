use std::{collections::HashMap, fs, iter::Extend};

use anyhow::{bail, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum},
};

use crate::{
    ast::{Expr, ExprKind},
    bindings::llvm_bindings::create_named_struct,
    codegen::{
        builtin::{
            import::{header::compile_header, resolve_lib::resolve_std_lib},
            BuiltinFunction,
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
    /// Imports a module referenced by an `@import` function call and returns an LLVM-visible module value.
    ///
    /// This function expects `expr` to be a `FunctionCall` whose single argument is a string literal naming the module to import.
    /// It determines the module type from the string (header vs. library/oxi) and delegates to the appropriate compilation path
    /// to produce a `SmartValue` that represents the imported module in the current LLVM/compilation context.
    ///
    /// # Errors
    ///
    /// Returns an error if the call expression does not have exactly one argument, if that argument is not a string literal,
    /// or if the delegated compilation/import operation fails.
    ///
    /// # Returns
    ///
    /// `SmartValue<'ctx>` representing the imported module's constant LLVM struct value.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// // Hypothetical usage (types and values are illustrative and omitted for brevity)
    /// let result = handle_call(context, module, builder, &expr, &mut compilation_context)?;
    /// // `result` is a SmartValue wrapping the imported module
    /// ```
    fn handle_call<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        expr: &Expr,
        compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<SmartValue<'ctx>> {
        let fn_expr = match &expr.kind {
            ExprKind::FunctionCall(fn_expr) => fn_expr,
            _ => unreachable!(),
        };

        // Resolve path
        if fn_expr.arguments.len() != 1 {
            bail!("Expected one argument to @import");
        }

        let module_name = match &fn_expr.arguments[0].kind {
            ExprKind::String(sym) => sym.value.clone(),
            _ => bail!("Expected string literal as argument to @import"),
        };

        match ModuleType::from(&module_name) {
            ModuleType::Library | ModuleType::Oxi => compile_oxi_module(
                context,
                module,
                builder,
                expr,
                module_name.into(),
                compilation_context,
            ),
            ModuleType::Header => compile_header(
                context,
                module,
                builder,
                module_name.into(),
                compilation_context,
            ),
        }
        // Resolve path
    }
}

/// Import and compile an Oxi (".oxi") or the special "std" module and produce an LLVM-visible module value.
///
/// Resolves the module path, parses and compiles the module into its own compilation context, and constructs a constant LLVM struct value representing the imported module for use by the caller.
///
/// # Examples
///
/// ```no_run
/// // Illustrative usage (types and setup omitted for brevity):
/// // let value = compile_oxi_module(&context, &module, &builder, &expr, "foo.oxi".to_string(), &mut compilation_context)?;
/// ```
///
/// # Returns
///
/// `SmartValue` wrapping a constant LLVM struct that represents the imported module.
fn compile_oxi_module<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    expr: &Expr,
    module_name: String,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    // Resolve path
    let module_path = if module_name == "std" {
        resolve_std_lib(expr.span, compilation_context.module_id)?
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

    let (tokens, module_id) = tokenize(file, &module_path)?;
    let ast = parse(tokens)?;

    let mut mod_compilation_context = CompilationContext::new(module_path, module_id);

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

/// Constructs an LLVM-visible module value representing the given compiled module and
/// registers its struct type and field indices in the caller's compilation context.
///
/// This transfers builtin symbols and builtin-like struct definitions from the imported
/// module into the caller's compilation context, creates a named LLVM struct type
/// "Module_<module_name>" (stripping a trailing ".oxi" from `module_name` if present),
/// populates that struct's fields from the imported module's exported symbols, and
/// returns a constant struct `SmartValue` containing those exports.
///
/// # Parameters
///
/// - `context`: LLVM `Context` used to create the module struct type and constant.
/// - `module_name`: The original module name (may include a trailing `.oxi`), used to
///   derive the LLVM struct name "Module_<module_name-without-.oxi>".
/// - `compilation_context`: The caller's mutable compilation context; the function will
///   extend its builtins and register the created module `StructDef` in its type context.
/// - `mod_compilation_context`: The compilation context of the imported module whose
///   exported symbols populate the module struct.
///
/// # Returns
///
/// A `SmartValue` wrapping a constant LLVM struct whose fields correspond to the imported
/// module's exported symbols.
///
/// # Examples
///
/// ```no_run
/// // Assume `ctx`, `caller_ctx`, and `imported_ctx` are prepared compilation contexts:
/// let sv = create_module(&ctx, "mylib.oxi".to_string(), &mut caller_ctx, imported_ctx)?;
/// // `sv` is a `SmartValue` representing the imported module's constant struct.
/// # Ok::<(), anyhow::Error>(())
/// ```
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
    let entries: Vec<(Box<str>, BasicTypeEnum)> = mod_compilation_context
        .symbol_table
        .iter()
        .map(|(name, entry)| (name.clone(), entry.value.value.get_type()))
        .collect();

    let module_name = format!(
        "Module_{}",
        module_name.strip_suffix(".oxi").unwrap_or(&module_name)
    );

    let values: Vec<BasicTypeEnum> = entries.iter().map(|(_, ty)| *ty).collect();

    let struct_ty = create_named_struct(context, &values, &module_name, false)?;

    let mut field_indices: HashMap<Box<str>, u32> = HashMap::new();
    for (i, (name, _ty)) in entries.iter().enumerate() {
        field_indices.insert(name.clone(), i as u32);
    }

    compilation_context.type_context.struct_defs.insert(
        module_name.into(),
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
    Oxi,
    Header,
}

impl From<&Box<str>> for ModuleType {
    /// Determines the module kind from a file path by its extension.
    ///
    /// Recognizes ".oxi" as an Oxi module, ".h" as a Header, and treats all other paths as a Library.
    ///
    /// # Examples
    ///
    /// ```
    /// # use crate::ModuleType;
    /// let t = ModuleType::from(&Box::from("core.oxi"));
    /// assert_eq!(t, ModuleType::Oxi);
    ///
    /// let h = ModuleType::from(&Box::from("bindings.h"));
    /// assert_eq!(h, ModuleType::Header);
    ///
    /// let lib = ModuleType::from(&Box::from("utils"));
    /// assert_eq!(lib, ModuleType::Library);
    /// ```
    fn from(path: &Box<str>) -> Self {
        if path.ends_with(".oxi") {
            ModuleType::Oxi
        } else if path.ends_with(".h") {
            ModuleType::Header
        } else {
            ModuleType::Library
        }
    }
}