use std::collections::HashMap;

use anyhow::Result;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, FunctionValue},
    AddressSpace,
};

use crate::{
    ast::{
        ast::{Statement, Type},
        statements::{BlockStmt, ExpressionStmt, FnDeclStmt, ReturnStmt},
    },
    intermediate::{
        builtin::slice::create_slice_struct, compile_expr::compile_expression_to_value,
    },
};

pub type SymbolTable<'ctx> = HashMap<String, BasicValueEnum<'ctx>>;

pub fn compile<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    ast: &BlockStmt,
    symbol_table: &mut SymbolTable<'ctx>,
) -> Result<()> {
    let mut function_table: HashMap<String, FunctionValue> = HashMap::new();

    let body = &ast.body;

    // Preprocess functions
    for stmt in body {
        if let Statement::FnDecl(fn_decl) = stmt {
            let param_types: Vec<Type> = fn_decl
                .arguments
                .iter()
                .map(|arg| arg.explicit_type.clone().unwrap())
                .collect();
            let fn_type = compile_function_type(context, &fn_decl.explicit_type, &param_types);

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
                compile_return(context, module, builder, ret_stmt, symbol_table)?;
            }
            Statement::Expression(expr_stmt) => {
                compile_expression(context, module, builder, expr_stmt, symbol_table)?;
            }
            Statement::VarDecl(var_decl) => {
                compile_var_decl(context, module, var_decl);
            }
            Statement::StructDecl(struct_decl) => {
                compile_struct_decl(context, module, struct_decl);
            }
            Statement::Block(block) => {
                let mut inner_symbol_table = symbol_table.clone();
                compile(context, module, builder, block, &mut inner_symbol_table)?;
            }
        }
    }

    Ok(())
}

fn compile_type<'ctx>(context: &'ctx Context, ty: &Type) -> BasicTypeEnum<'ctx> {
    match ty {
        Type::Symbol(sym) => match sym.name.as_str() {
            "i32" => context.i32_type().as_basic_type_enum(),
            "u8" => context.i8_type().as_basic_type_enum(),
            "usize" => context.i64_type().as_basic_type_enum(),
            tyname => unimplemented!("{tyname}"),
        },
        Type::Slice(slice_ty) => {
            let element_ty = compile_type(context, &slice_ty.underlying);
            create_slice_struct(context, element_ty).as_basic_type_enum()
        }
        ty => unimplemented!("{ty:#?}"),
    }
}

fn compile_function_type<'ctx>(
    context: &'ctx Context,
    return_type: &Type,
    param_types: &[Type],
) -> FunctionType<'ctx> {
    let return_llvm_type: Option<BasicTypeEnum> = match return_type {
        Type::Symbol(sym) if sym.name == "void" => None,
        _ => Some(compile_type(context, return_type)),
    };

    let param_llvm_types: Vec<_> = param_types
        .iter()
        .map(|ty| compile_type(context, ty).into())
        .collect();

    if let Some(ret_ty) = return_llvm_type {
        ret_ty.fn_type(&param_llvm_types, false)
    } else {
        context.void_type().fn_type(&param_llvm_types, false)
    }
}

fn compile_function<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    function: FunctionValue<'ctx>,
    fn_decl: &FnDeclStmt,
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
    compile(context, module, &builder, &body, &mut symbol_table)?;

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
    symbol_table: &mut SymbolTable<'ctx>,
) -> Result<()> {
    if let Some(expr) = &ret_stmt.value {
        let ret_val: BasicValueEnum =
            compile_expression_to_value(context, module, builder, expr, symbol_table)?;
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
    symbol_table: &mut SymbolTable<'ctx>,
) -> Result<()> {
    compile_expression_to_value(
        context,
        module,
        builder,
        &expr_stmt.expression,
        symbol_table,
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

fn compile_struct_decl(
    context: &Context,
    module: &Module,
    struct_decl: &crate::ast::statements::StructDeclStmt,
) {
    // TODO: Implement struct declaration compilation
}
