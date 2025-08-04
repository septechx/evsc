use std::collections::HashMap;

use anyhow::Result;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, FunctionValue},
};

use crate::{
    ast::{
        ast::{Statement, Type},
        statements::{BlockStmt, ExpressionStmt, FnDeclStmt, ReturnStmt, StructDeclStmt},
    },
    intermediate::{
        compile_expr::compile_expression_to_value,
        compile_type::{compile_function_type, compile_type},
    },
};

pub type SymbolTable<'ctx> = HashMap<String, BasicValueEnum<'ctx>>;

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
                compile_var_decl(context, module, var_decl);
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
    for (i, arg_decl) in fn_decl.arguments.iter().enumerate() {
        if let Some(param) = function.get_nth_param(i as u32) {
            param.set_name(&arg_decl.name);
            symbol_table.insert(arg_decl.name.clone(), param);
        }
    }

    let entry_bb = context.append_basic_block(function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(entry_bb);

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
        let ret_val: BasicValueEnum =
            compile_expression_to_value(context, module, builder, expr, compilation_context)?;
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

fn compile_var_decl(
    context: &Context,
    module: &Module,
    var_decl: &crate::ast::statements::VarDeclStmt,
) {
    // TODO: Implement variable declaration compilation
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
