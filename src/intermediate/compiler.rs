use anyhow::Result;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, BasicValueEnum},
};

use crate::ast::{ast::Statement, statements::BlockStmt};

pub fn compile<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    ast: &BlockStmt,
) -> Result<()> {
    let body = &ast.body;
    for stmt in body {
        match stmt {
            Statement::FnDecl(fn_decl) => {
                compile_function(context, module, fn_decl)?;
            }
            Statement::Return(ret_stmt) => {
                compile_return(context, module, builder, ret_stmt)?;
            }
            Statement::Expression(expr_stmt) => {
                compile_expression(context, module, expr_stmt);
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

fn compile_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    fn_decl: &crate::ast::statements::FnDeclStmt,
) -> Result<()> {
    // Here we hard‑code a void function returning i32 for demo;
    // we’d pull the type from fn_decl.explicit_type, etc.
    let fn_type = context.i32_type().fn_type(&[], false);
    let function = module.add_function(&fn_decl.name, fn_type, None);

    // create an “entry” block and a builder
    let entry_bb = context.append_basic_block(function, "entry");
    let builder = context.create_builder();
    builder.position_at_end(entry_bb);

    // recurse into the body, passing along our builder
    let body = BlockStmt {
        body: fn_decl.body.clone(),
    };
    compile(context, module, &builder, &body)?;

    // if the user forgot to return, insert a default one:
    if function
        .get_last_basic_block()
        .unwrap()
        .get_terminator()
        .is_none()
    {
        builder.build_return(Some(&context.i32_type().const_int(0, false)))?;
    }

    Ok(())
}

/// Compile a `return`. If `value` is `Some(expr)`, compile it to an LLVM value
/// and `ret value`; otherwise `ret void`.
fn compile_return<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    ret_stmt: &crate::ast::statements::ReturnStmt,
) -> Result<()> {
    if let Some(expr) = &ret_stmt.value {
        // assume compile_expression returns a BasicValueEnum
        let ret_val: BasicValueEnum = compile_expression_to_value(context, module, builder, expr);
        builder.build_return(Some(&ret_val))?;
    } else {
        builder.build_return(None)?;
    }

    Ok(())
}

/// Example stub: turns an AST expression into an LLVM value.
/// We’ll need to implement this for all our expr variants.
fn compile_expression_to_value<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    expr: &crate::ast::ast::Expression,
) -> BasicValueEnum<'ctx> {
    match expr {
        crate::ast::ast::Expression::Number(n) => context
            .i32_type()
            .const_int(n.value as u64, false)
            .as_basic_value_enum(),
        _ => unimplemented!(),
    }
}

fn compile_expression(
    context: &Context,
    module: &Module,
    expr_stmt: &crate::ast::statements::ExpressionStmt,
) {
    // TODO: Implement expression compilation
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
