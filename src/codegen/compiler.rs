use std::path::PathBuf;

use anyhow::{Result, bail};
use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue},
};

use crate::{
    ast::{
        ExprKind, Stmt, StmtKind, Type,
        statements::{ExpressionStmt, FnDeclStmt, ReturnStmt, StructDeclStmt, VarDeclStmt},
    },
    bindings::llvm_bindings::create_named_struct,
    codegen::{
        builtin::Builtin,
        compile_expr::compile_expression_to_value,
        compile_type::{cast_int_to_type, compile_function_type, compile_type},
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

pub fn compile_stmts<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    stmts: &[Stmt],
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    // 1st pass: Declare functions and structs
    for stmt in stmts {
        match &stmt.kind {
            StmtKind::FnDecl(fn_decl) => {
                let param_types: Vec<Type> = fn_decl
                    .arguments
                    .iter()
                    .map(|arg| arg.type_.clone())
                    .collect();
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
            StmtKind::StructDecl(struct_decl) => {
                compile_struct_decl(context, module, struct_decl, compilation_context)?;
            }
            _ => (),
        }
    }

    // 2nd pass: Compile block
    for stmt in stmts {
        match &stmt.kind {
            StmtKind::FnDecl(fn_decl) => {
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
            StmtKind::Return(ret_stmt) => {
                compile_return(context, module, builder, ret_stmt, compilation_context)?;
            }
            StmtKind::Expression(expr_stmt) => {
                compile_expression(context, module, builder, expr_stmt, compilation_context)?;
            }
            StmtKind::VarDecl(var_decl) => {
                compile_var_decl(context, module, builder, var_decl, compilation_context)?;
            }
            StmtKind::StructDecl(_) => (), // Structs are compiled during the first pass
            StmtKind::InterfaceDecl(_) => todo!(),
            StmtKind::Import(_) => todo!(),
        }
    }

    Ok(())
}

fn compile_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    function: FunctionValue<'ctx>,
    fn_decl: &FnDeclStmt,
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

    for (i, arg_decl) in fn_decl.arguments.iter().enumerate() {
        if let Some(param) = function.get_nth_param(i as u32) {
            param.set_name(&arg_decl.name.value);

            let alloca = builder.build_alloca(param.get_type(), &arg_decl.name.value)?;
            builder.build_store(alloca, param)?;

            symbol_table.insert(
                arg_decl.name.value.clone(),
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
        match &body.kind {
            ExprKind::Block(block) => {
                compile_stmts(
                    context,
                    module,
                    &builder,
                    &block.body,
                    &mut inner_compilation_context,
                )?;
            }
            _ => bail!("Expected block expression, got {:?}", body.kind),
        }
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

fn compile_return<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    ret_stmt: &ReturnStmt,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    if let Some(expr) = &ret_stmt.value {
        let ret = compile_expression_to_value(context, module, builder, expr, compilation_context)?;

        let ret_val = ret.unwrap(builder)?;

        let function = builder
            .get_insert_block()
            .expect("function has a block")
            .get_parent()
            .expect("block has a function");
        let expected_ret_type = function.get_type().get_return_type();

        if let Some(expected_type) = expected_ret_type {
            let casted = cast_int_to_type(builder, ret_val, expected_type)?;
            builder.build_return(Some(&casted))?;
        } else {
            builder.build_return(Some(&ret_val))?;
        }
    } else {
        builder.build_return(None)?;
    }

    Ok(())
}

fn compile_expression<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    expr_stmt: &'a ExpressionStmt,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    compile_expression_to_value(
        context,
        module,
        builder,
        &expr_stmt.expression,
        compilation_context,
    )?;
    Ok(())
}

fn compile_var_decl<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    var_decl: &VarDeclStmt,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    let value = if let Some(expr) = &var_decl.assigned_value {
        compile_expression_to_value(context, module, builder, expr, compilation_context)?
    } else {
        bail!("Variable must have an initial value");
    };

    let value = value.unwrap(builder)?;

    if var_decl.is_static {
        let gv = add_global_constant(
            module,
            value.get_type(),
            &var_decl.variable_name.value,
            value,
        )?;
        gv.set_linkage(Linkage::Private);

        compilation_context.symbol_table.insert(
            var_decl.variable_name.value.clone(),
            SymbolTableEntry::from_pointer(
                context,
                gv.as_pointer_value().as_basic_value_enum(),
                value.get_type(),
            ),
        );

        return Ok(());
    }

    let alloca = builder.build_alloca(value.get_type(), &var_decl.variable_name.value)?;
    builder.build_store(alloca, value)?;

    compilation_context.symbol_table.insert(
        var_decl.variable_name.value.clone(),
        SymbolTableEntry::from_pointer(context, alloca.as_basic_value_enum(), value.get_type()),
    );

    Ok(())
}

fn compile_struct_decl<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    struct_decl: &StructDeclStmt,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    let mut field_types = Vec::new();
    let mut field_indices = FxHashMap::default();

    for (index, property) in struct_decl.properties.iter().enumerate() {
        let field_ty = compile_type(context, &property.type_, compilation_context)?;
        field_types.push(field_ty);
        field_indices.insert(property.name.value.clone(), index as u32);
    }

    for method in &struct_decl.methods {
        let mut method = method.fn_decl.clone();
        method.name.value = format!("{}_{}", struct_decl.name.value, method.name.value).into();

        let param_types: Vec<Type> = method
            .arguments
            .iter()
            .map(|arg| arg.type_.clone())
            .collect();
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

    let struct_ty = create_named_struct(context, &field_types, &struct_decl.name.value, false)?;

    compilation_context.type_context.struct_defs.insert(
        struct_decl.name.value.clone(),
        StructDef {
            llvm_type: struct_ty,
            is_builtin: false,
            field_indices,
        },
    );

    Ok(())
}
