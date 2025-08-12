use std::{collections::HashMap, path::PathBuf};

use anyhow::{bail, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue},
    AddressSpace,
};

use crate::{
    ast::{
        ast::{Statement, Type},
        statements::{
            BlockStmt, ExpressionStmt, FnDeclStmt, ReturnStmt, StructDeclStmt, VarDeclStmt,
        },
    },
    bindings::llvm_bindings::create_named_struct,
    intermediate::{
        compile_expr::compile_expression_to_value,
        compile_type::{compile_function_type, compile_type},
        pointer::{get_value, SmartValue},
    },
};

pub type SymbolTable<'ctx> = HashMap<String, SymbolTableEntry<'ctx>>;

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
    pub struct_defs: HashMap<String, StructDef<'ctx>>,
}

#[derive(Clone, Debug)]
pub struct StructDef<'ctx> {
    pub llvm_type: inkwell::types::StructType<'ctx>,
    pub field_indices: HashMap<String, u32>,
}

#[derive(Clone, Debug, Default)]
pub struct CompilationContext<'ctx> {
    pub symbol_table: SymbolTable<'ctx>,
    pub type_context: TypeContext<'ctx>,
    pub module_path: PathBuf,
}

impl<'ctx> CompilationContext<'ctx> {
    pub fn new(path: PathBuf) -> Self {
        CompilationContext {
            symbol_table: HashMap::new(),
            type_context: TypeContext::default(),
            module_path: path,
        }
    }
}

pub fn compile<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    ast: &BlockStmt,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    let mut function_table: HashMap<String, FunctionValue> = HashMap::new();

    let body = &ast.body;

    // 1st pass: Declare functions and structs
    for stmt in body {
        match stmt {
            Statement::FnDecl(fn_decl) => {
                let param_types: Vec<Type> = fn_decl
                    .arguments
                    .iter()
                    .map(|arg| arg.explicit_type.clone().unwrap())
                    .collect();
                let fn_type = compile_function_type(
                    context,
                    &fn_decl.explicit_type,
                    &param_types,
                    compilation_context,
                );

                let function = module.add_function(&fn_decl.name, fn_type, None);
                function_table.insert(fn_decl.name.clone(), function);
            }
            Statement::StructDecl(struct_decl) => {
                compile_struct_decl(context, struct_decl, compilation_context)?;
            }
            _ => (),
        }
    }

    // 2nd pass: Compile block
    for stmt in body {
        match stmt {
            Statement::FnDecl(fn_decl) => {
                if let Some(function) = function_table.get(&fn_decl.name) {
                    compile_function(context, module, *function, fn_decl, compilation_context)?;
                }
            }
            Statement::Return(ret_stmt) => {
                compile_return(context, module, builder, ret_stmt, compilation_context)?;
            }
            Statement::Expression(expr_stmt) => {
                compile_expression(context, module, builder, expr_stmt, compilation_context)?;
            }
            Statement::VarDecl(var_decl) => {
                compile_var_decl(context, module, builder, var_decl, compilation_context)?;
            }
            Statement::Block(block) => {
                let mut inner_compilation_context = compilation_context.clone();
                compile(
                    context,
                    module,
                    builder,
                    block,
                    &mut inner_compilation_context,
                )?;
            }
            Statement::StructDecl(_) => (), // Structs are compiled during the first pass
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
    let mut symbol_table: HashMap<String, SymbolTableEntry> = HashMap::new();

    let entry_bb = context.append_basic_block(function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(entry_bb);

    for (i, arg_decl) in fn_decl.arguments.iter().enumerate() {
        if let Some(param) = function.get_nth_param(i as u32) {
            param.set_name(&arg_decl.name);

            let alloca = builder.build_alloca(param.get_type(), &arg_decl.name)?;
            builder.build_store(alloca, param)?;

            symbol_table.insert(
                arg_decl.name.clone(),
                SymbolTableEntry::from_pointer(
                    context,
                    alloca.as_basic_value_enum(),
                    param.get_type(),
                ),
            );
        }
    }

    let body = BlockStmt {
        body: fn_decl.body.clone(),
    };

    let mut inner_compilation_context = compilation_context.clone();
    inner_compilation_context.symbol_table.extend(symbol_table);

    compile(
        context,
        module,
        &builder,
        &body,
        &mut inner_compilation_context,
    )?;

    if function
        .get_last_basic_block()
        .unwrap()
        .get_terminator()
        .is_none()
    {
        builder.build_return(None)?;
    }

    compilation_context.symbol_table.insert(
        fn_decl.name.clone(),
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

        let ret_val = get_value(builder, &ret)?;

        builder.build_return(Some(&ret_val))?;
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

    let value = get_value(builder, &value)?;

    let alloca = builder.build_alloca(value.get_type(), &var_decl.variable_name)?;
    builder.build_store(alloca, value)?;

    compilation_context.symbol_table.insert(
        var_decl.variable_name.clone(),
        SymbolTableEntry::from_pointer(context, alloca.as_basic_value_enum(), value.get_type()),
    );

    Ok(())
}

fn compile_struct_decl<'ctx>(
    context: &'ctx Context,
    struct_decl: &StructDeclStmt,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    let mut field_types = Vec::new();
    let mut field_indices = HashMap::new();

    for (index, property) in struct_decl.properties.iter().enumerate() {
        let field_ty = compile_type(context, &property.explicit_type, compilation_context);
        field_types.push(field_ty);
        field_indices.insert(property.name.clone(), index as u32);
    }

    let struct_ty = create_named_struct(context, &field_types, &struct_decl.name, false)?;

    compilation_context.type_context.struct_defs.insert(
        struct_decl.name.clone(),
        StructDef {
            llvm_type: struct_ty,
            field_indices,
        },
    );

    Ok(())
}
