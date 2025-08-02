use std::collections::HashMap;

use anyhow::Result;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::FunctionType,
    values::{BasicValueEnum, FunctionValue},
};

use crate::{
    ast::{
        ast::{Statement, Type},
        statements::{BlockStmt, ExpressionStmt, FnDeclStmt, ReturnStmt},
    },
    intermediate::compile_expr::compile_expression_to_value,
};

pub fn compile<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    ast: &BlockStmt,
) -> Result<()> {
    let mut function_table: HashMap<String, FunctionValue> = HashMap::new();

    let body = &ast.body;

    // Preprocess functions
    for stmt in body {
        if let Statement::FnDecl(fn_decl) = stmt {
            let fn_type = compile_fn_type(context, &fn_decl.explicit_type.clone());

            let function = module.add_function(&fn_decl.name, fn_type, None);
            function_table.insert(fn_decl.name.clone(), function);
        }
    }

    // Compile block
    for stmt in body {
        match stmt {
            Statement::FnDecl(fn_decl) => {
                if let Some(function) = function_table.get(&fn_decl.name) {
                    compile_function(context, module, *function, fn_decl)?;
                }
            }
            Statement::Return(ret_stmt) => {
                compile_return(context, module, builder, ret_stmt)?;
            }
            Statement::Expression(expr_stmt) => {
                compile_expression(context, module, builder, expr_stmt)?;
            }
            Statement::VarDecl(var_decl) => {
                compile_var_decl(context, module, var_decl);
            }
            Statement::StructDecl(struct_decl) => {
                compile_struct_decl(context, module, struct_decl);
            }
            Statement::Block(block) => {
                compile(context, module, builder, block)?;
            }
        }
    }

    Ok(())
}

fn compile_fn_type<'ctx>(context: &'ctx Context, fn_type: &Type) -> FunctionType<'ctx> {
    match fn_type {
        Type::Symbol(symbol_type) => match symbol_type.name.as_str() {
            "i32" => context.i32_type().fn_type(&[], false),
            "void" => context.void_type().fn_type(&[], false),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn compile_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    function: FunctionValue<'ctx>,
    fn_decl: &FnDeclStmt,
) -> Result<()> {
    let entry_bb = context.append_basic_block(function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(entry_bb);

    let body = BlockStmt {
        body: fn_decl.body.clone(),
    };
    compile(context, module, &builder, &body)?;

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
) -> Result<()> {
    if let Some(expr) = &ret_stmt.value {
        let ret_val: BasicValueEnum = compile_expression_to_value(context, module, builder, expr)?;
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
) -> Result<()> {
    compile_expression_to_value(context, module, builder, &expr_stmt.expression)?;
    Ok(())
}

fn compile_var_decl(
    context: &Context,
    module: &Module,
    var_decl: &crate::ast::statements::VarDeclStmt,
) {
    // TODO: Implement variable declaration compilation
}

fn compile_struct_decl(
    context: &Context,
    module: &Module,
    struct_decl: &crate::ast::statements::StructDeclStmt,
) {
    // TODO: Implement struct declaration compilation
}
