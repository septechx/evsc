use std::collections::HashMap;

use anyhow::{bail, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, PointerType},
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
    intermediate::{
        compile_expr::compile_expression_to_value,
        compile_type::{compile_function_type, compile_type},
    },
};

pub type SymbolTable<'ctx> = HashMap<String, SymbolTableEntry<'ctx>>;

#[derive(Clone, Debug)]
pub struct SymbolTableEntry<'ctx> {
    pub value: SmartValue<'ctx>,
    pub ty: BasicTypeEnum<'ctx>,
}

#[derive(Clone, Debug)]
pub struct SmartValue<'ctx> {
    pub value: BasicValueEnum<'ctx>,
    pub pointee_ty: Option<BasicTypeEnum<'ctx>>,
}

impl<'ctx> SmartValue<'ctx> {
    pub fn from_value(value: BasicValueEnum<'ctx>) -> Self {
        Self {
            value,
            pointee_ty: None,
        }
    }

    pub fn from_pointer(value: BasicValueEnum<'ctx>, pointee_ty: BasicTypeEnum<'ctx>) -> Self {
        Self {
            value,
            pointee_ty: Some(pointee_ty),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeContext<'ctx> {
    pub struct_defs: HashMap<String, StructDef<'ctx>>,
}

impl Default for TypeContext<'_> {
    fn default() -> Self {
        Self {
            struct_defs: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StructDef<'ctx> {
    pub llvm_type: inkwell::types::StructType<'ctx>,
    pub field_indices: HashMap<String, u32>,
}

#[derive(Clone, Debug)]
pub struct CompilationContext<'ctx> {
    pub symbol_table: SymbolTable<'ctx>,
    pub type_context: TypeContext<'ctx>,
}

impl Default for CompilationContext<'_> {
    fn default() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            type_context: TypeContext::default(),
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

    // Preprocess functions
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
                compile_struct_decl(context, struct_decl, compilation_context);
            }
            _ => (),
        }
    }

    // Compile block
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
            Statement::StructDecl(struct_decl) => {
                compile_struct_decl(context, struct_decl, compilation_context);
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
    let mut symbol_table: HashMap<String, BasicValueEnum> = HashMap::new();

    let entry_bb = context.append_basic_block(function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(entry_bb);

    for (i, arg_decl) in fn_decl.arguments.iter().enumerate() {
        if let Some(param) = function.get_nth_param(i as u32) {
            param.set_name(&arg_decl.name);

            let alloca = builder.build_alloca(param.get_type(), &arg_decl.name)?;
            builder.build_store(alloca, param)?;

            symbol_table.insert(arg_decl.name.clone(), alloca.as_basic_value_enum());
        }
    }

    let body = BlockStmt {
        body: fn_decl.body.clone(),
    };
    compile(context, module, &builder, &body, compilation_context)?;

    if function
        .get_last_basic_block()
        .unwrap()
        .get_terminator()
        .is_none()
    {
        builder.build_return(None)?;
    }

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
        let ret_ptr = ret.value.into_pointer_value();
        let ret_val = builder.build_load(ret.pointee_ty.unwrap(), ret_ptr, "ret")?;

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

    let alloca = builder.build_alloca(value.value.get_type(), &var_decl.variable_name)?;
    builder.build_store(alloca, value.value)?;

    compilation_context.symbol_table.insert(
        var_decl.variable_name.clone(),
        SymbolTableEntry {
            value: SmartValue::from_pointer(alloca.as_basic_value_enum(), value.value.get_type()),
            ty: context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
        },
    );

    Ok(())
}

fn compile_struct_decl<'ctx>(
    context: &'ctx Context,
    struct_decl: &StructDeclStmt,
    compilation_context: &mut CompilationContext<'ctx>,
) {
    let mut field_types = Vec::new();
    let mut field_indices = HashMap::new();

    for (index, property) in struct_decl.properties.iter().enumerate() {
        let field_ty = compile_type(context, &property.explicit_type, compilation_context);
        field_types.push(field_ty);
        field_indices.insert(property.name.clone(), index as u32);
    }

    let struct_ty = context.opaque_struct_type(&struct_decl.name);
    struct_ty.set_body(&field_types, false);

    compilation_context.type_context.struct_defs.insert(
        struct_decl.name.clone(),
        StructDef {
            llvm_type: struct_ty,
            field_indices,
        },
    );
}
