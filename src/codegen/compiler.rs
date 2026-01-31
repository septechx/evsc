use std::path::PathBuf;

use anyhow::{bail, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue},
    AddressSpace,
};
use thin_vec::ThinVec;

use crate::{
    ast::{
        AssocItem, AssocItemKind, Expr, Fn, Ident, Item, ItemKind, Mutability, Stmt, StmtKind,
        Type, Visibility,
    },
    bindings::llvm_bindings::create_named_struct,
    codegen::{
        builtin::Builtin,
        compile_expr::compile_expression_to_value,
        compile_type::{compile_function_type, compile_type},
        inkwell_ext::add_global_constant,
        pointer::SmartValue,
    },
    hashmap::{FxHashMap, FxHashSet},
    span::ModuleId,
};

pub type FunctionTable<'ctx> = FxHashMap<Box<str>, FunctionTableEntry<'ctx>>;
pub type SymbolTable<'ctx> = FxHashMap<Box<str>, SymbolTableEntry<'ctx>>;

#[derive(Clone, Debug)]
pub struct FunctionTableEntry<'ctx> {
    pub function: FunctionValue<'ctx>,
    pub is_builtin: bool,
}

impl<'ctx> From<FunctionValue<'ctx>> for FunctionTableEntry<'ctx> {
    fn from(function: FunctionValue<'ctx>) -> Self {
        Self {
            function,
            is_builtin: false,
        }
    }
}

impl<'ctx> From<FunctionTableEntry<'ctx>> for FunctionValue<'ctx> {
    fn from(entry: FunctionTableEntry<'ctx>) -> Self {
        entry.function
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct SymbolTableEntry<'ctx> {
    pub value: SmartValue<'ctx>,
    pub ty: BasicTypeEnum<'ctx>,
}

impl<'ctx> SymbolTableEntry<'ctx> {
    pub fn from_pointer(
        context: &'ctx Context,
        value: BasicValueEnum<'ctx>,
        ty: BasicTypeEnum<'ctx>,
    ) -> Self {
        let value = SmartValue::from_pointer(value, ty);
        Self {
            value,
            ty: context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
        }
    }

    pub fn from_value(value: BasicValueEnum<'ctx>, ty: BasicTypeEnum<'ctx>) -> Self {
        let value = SmartValue::from_value(value);
        Self { value, ty }
    }
}

#[derive(Clone, Debug, Default)]
pub struct TypeContext<'ctx> {
    pub struct_defs: FxHashMap<Box<str>, StructDef<'ctx>>,
}

#[derive(Clone, Debug)]
pub struct StructDef<'ctx> {
    pub llvm_type: inkwell::types::StructType<'ctx>,
    pub field_indices: FxHashMap<Box<str>, u32>,
    pub is_builtin: bool,
}

#[derive(Clone, Debug, Default)]
pub struct CompilationContext<'ctx> {
    /// Stores all symbols (functions, globals)
    pub symbol_table: SymbolTable<'ctx>,
    /// Stores all function declarations
    pub function_table: FunctionTable<'ctx>,
    /// Stores all type declarations (structs)
    pub type_context: TypeContext<'ctx>,
    /// Keeps track of all used builtins, currently unused
    pub builtins: FxHashSet<Builtin>,
    pub module_path: PathBuf,
    pub module_id: ModuleId,
}

impl<'ctx> CompilationContext<'ctx> {
    pub fn new(path: PathBuf, module_id: ModuleId) -> Self {
        CompilationContext {
            symbol_table: FxHashMap::default(),
            function_table: FxHashMap::default(),
            type_context: TypeContext::default(),
            builtins: FxHashSet::default(),
            module_path: path,
            module_id,
        }
    }
}

pub fn compile_items<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    items: &[Item],
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    // 1st pass: Declare functions and structs
    for item in items {
        match &item.kind {
            ItemKind::Fn(fn_decl) => {
                let param_types: Vec<Type> =
                    fn_decl.parameters.iter().map(|arg| arg.1.clone()).collect();
                let fn_type = compile_function_type(
                    context,
                    &fn_decl.return_type,
                    &param_types,
                    compilation_context,
                )?;

                let function = module.add_function(&fn_decl.name.value, fn_type, None);
                compilation_context
                    .function_table
                    .insert(fn_decl.name.value.clone(), function.into());
            }
            ItemKind::Struct {
                name,
                fields,
                items,
            } => {
                compile_struct_decl(context, module, name, fields, items, compilation_context)?;
            }
            _ => (),
        }
    }

    // 2nd pass: Compile function bodies and static variables
    for item in items {
        match &item.kind {
            ItemKind::Fn(fn_decl) => {
                if let Some(function) = compilation_context
                    .function_table
                    .get(fn_decl.name.value.as_ref())
                {
                    compile_function(
                        context,
                        module,
                        function.clone().into(),
                        fn_decl,
                        compilation_context,
                    )?;
                }
            }
            ItemKind::Static { name, ty, value } => {
                compile_static_item(
                    context,
                    module,
                    builder,
                    name,
                    ty,
                    value,
                    item.visibility,
                    compilation_context,
                )?;
            }
            ItemKind::Struct { .. } => (), // Structs are compiled during the first pass
            ItemKind::Interface { .. } => todo!(),
            ItemKind::Impl { .. } => todo!(),
            ItemKind::Import(_) => todo!(),
        }
    }

    Ok(())
}

pub fn compile_body_stmts<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    stmts: &[Stmt],
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    for stmt in stmts {
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                compile_expression(context, module, builder, expr, compilation_context)?;
            }
            StmtKind::Semi(expr) => {
                compile_expression_to_value(context, module, builder, expr, compilation_context)?;
            }
            StmtKind::Let {
                name,
                ty,
                value,
                mutability,
            } => {
                compile_let_stmt(
                    context,
                    module,
                    builder,
                    name,
                    ty,
                    value.as_ref(),
                    *mutability,
                    compilation_context,
                )?;
            }
        }
    }

    Ok(())
}

fn compile_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    function: FunctionValue<'ctx>,
    fn_decl: &Fn,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    compilation_context.symbol_table.insert(
        fn_decl.name.value.clone(),
        SymbolTableEntry::from_value(
            function.as_global_value().as_basic_value_enum(),
            function
                .as_global_value()
                .as_pointer_value()
                .get_type()
                .as_basic_type_enum(),
        ),
    );

    if fn_decl.is_extern {
        return Ok(());
    }

    let mut symbol_table: FxHashMap<Box<str>, SymbolTableEntry> = FxHashMap::default();

    let entry_bb = context.append_basic_block(function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(entry_bb);

    for (i, arg_decl) in fn_decl.parameters.iter().enumerate() {
        if let Some(param) = function.get_nth_param(i as u32) {
            param.set_name(&arg_decl.0.value);

            let alloca = builder.build_alloca(param.get_type(), &arg_decl.0.value)?;
            builder.build_store(alloca, param)?;

            symbol_table.insert(
                arg_decl.0.value.clone(),
                SymbolTableEntry::from_pointer(
                    context,
                    alloca.as_basic_value_enum(),
                    param.get_type(),
                ),
            );
        }
    }

    let mut inner_compilation_context = compilation_context.clone();
    inner_compilation_context.symbol_table.extend(symbol_table);

    if let Some(body) = &fn_decl.body {
        compile_body_stmts(
            context,
            module,
            &builder,
            &body.stmts,
            &mut inner_compilation_context,
        )?;
    }

    if function
        .get_last_basic_block()
        .expect("function has a block")
        .get_terminator()
        .is_none()
    {
        builder.build_return(None)?;
    }

    compilation_context.symbol_table.insert(
        fn_decl.name.value.clone(),
        SymbolTableEntry::from_value(
            function.as_global_value().as_basic_value_enum(),
            function
                .as_global_value()
                .as_pointer_value()
                .get_type()
                .as_basic_type_enum(),
        ),
    );

    Ok(())
}

fn compile_expression<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    expr: &'a Expr,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    compile_expression_to_value(context, module, builder, expr, compilation_context)?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_let_stmt<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    name: &Ident,
    _ty: &Type,
    value: Option<&Expr>,
    _mutability: Mutability,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    let compiled_value = if let Some(expr) = value {
        compile_expression_to_value(context, module, builder, expr, compilation_context)?
    } else {
        bail!("Variable must have an initial value");
    };

    let compiled_value = compiled_value.unwrap(builder)?;

    let alloca = builder.build_alloca(compiled_value.get_type(), &name.value)?;
    builder.build_store(alloca, compiled_value)?;

    compilation_context.symbol_table.insert(
        name.value.clone(),
        SymbolTableEntry::from_pointer(
            context,
            alloca.as_basic_value_enum(),
            compiled_value.get_type(),
        ),
    );

    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_static_item<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    _builder: &'a Builder<'ctx>,
    static_name: &Ident,
    _static_ty: &Type,
    static_value: &Expr,
    visibility: Visibility,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    let value =
        compile_expression_to_value(context, module, _builder, static_value, compilation_context)?;

    let value = value.unwrap(_builder)?;

    let gv = add_global_constant(module, value.get_type(), &static_name.value, value)?;
    let linkage = match visibility {
        Visibility::Public => Linkage::External,
        Visibility::Private => Linkage::Private,
    };
    gv.set_linkage(linkage);

    compilation_context.symbol_table.insert(
        static_name.value.clone(),
        SymbolTableEntry::from_pointer(
            context,
            gv.as_pointer_value().as_basic_value_enum(),
            value.get_type(),
        ),
    );

    Ok(())
}

fn compile_struct_decl<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    struct_name: &Ident,
    struct_fields: &ThinVec<(Ident, Type, Visibility)>,
    struct_items: &ThinVec<AssocItem>,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    let mut field_types = Vec::new();
    let mut field_indices = FxHashMap::default();

    for (index, field) in struct_fields.iter().enumerate() {
        let field_ty = compile_type(context, &field.1, compilation_context)?;
        field_types.push(field_ty);
        field_indices.insert(field.0.value.clone(), index as u32);
    }

    for item in struct_items {
        let AssocItemKind::Fn(fn_decl) = &item.kind;
        let mut method = fn_decl.clone();
        method.name.value = format!("{}_{}", struct_name.value, method.name.value).into();

        let param_types: Vec<Type> = method.parameters.iter().map(|arg| arg.1.clone()).collect();
        let fn_type = compile_function_type(
            context,
            &method.return_type,
            &param_types,
            compilation_context,
        )?;

        let function = module.add_function(&method.name.value, fn_type, None);
        compilation_context
            .function_table
            .insert(method.name.value.clone(), function.into());

        compile_function(context, module, function, &method, compilation_context)?;
    }

    let struct_ty = create_named_struct(context, &field_types, &struct_name.value, false)?;

    compilation_context.type_context.struct_defs.insert(
        struct_name.value.clone(),
        StructDef {
            llvm_type: struct_ty,
            is_builtin: false,
            field_indices,
        },
    );

    Ok(())
}
