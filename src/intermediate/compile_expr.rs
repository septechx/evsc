use anyhow::{anyhow, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
    AddressSpace,
};

use crate::{
    ast::ast::Expression,
    intermediate::{builtin, compiler::SymbolTable},
    lexer::token::TokenKind,
};

pub fn compile_expression_to_value<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    expr: &Expression,
    symbol_table: &SymbolTable<'ctx>,
) -> Result<BasicValueEnum<'ctx>> {
    Ok(match expr {
        Expression::Number(n) => context
            .i32_type()
            .const_int(n.value as u64, false)
            .as_basic_value_enum(),
        Expression::String(s) => {
            let string_val = context.const_string(s.value.as_bytes(), false);
            let global = module.add_global(string_val.get_type(), None, "str");
            global.set_initializer(&string_val);
            global.set_constant(true);
            global.set_linkage(inkwell::module::Linkage::Private);
            let ptr = builder.build_pointer_cast(
                global.as_pointer_value(),
                context.i8_type().ptr_type(AddressSpace::default()),
                "string_ptr",
            )?;
            ptr.as_basic_value_enum()
        }
        Expression::Symbol(sym) => symbol_table
            .get(&sym.value)
            .cloned()
            .ok_or_else(|| anyhow!("Undefined variable `{}`", sym.value))?,
        Expression::Prefix(expr) => {
            let right =
                compile_expression_to_value(context, module, builder, &expr.right, symbol_table)?;

            match &expr.operator.kind {
                TokenKind::Reference => {
                    let ptr = builder.build_alloca(right.get_type(), "ptr")?;
                    builder.build_store(ptr, right)?;
                    ptr.as_basic_value_enum()
                }
                _ => unimplemented!(),
            }
        }
        Expression::Binary(expr) => {
            let left =
                compile_expression_to_value(context, module, builder, &expr.left, symbol_table)?;
            let right =
                compile_expression_to_value(context, module, builder, &expr.right, symbol_table)?;

            let left = left.into_int_value();
            let right = right.into_int_value();

            match &expr.operator.kind {
                TokenKind::Plus => {
                    let sum = builder.build_int_add(left, right, "sumtmp")?;
                    sum.as_basic_value_enum()
                }
                TokenKind::Dash => {
                    let sub = builder.build_int_sub(left, right, "subtmp")?;
                    sub.as_basic_value_enum()
                }
                TokenKind::Star => {
                    let mul = builder.build_int_mul(left, right, "multmp")?;
                    mul.as_basic_value_enum()
                }
                TokenKind::Slash => {
                    let div = builder.build_int_signed_div(left, right, "divtmp")?;
                    div.as_basic_value_enum()
                }
                TokenKind::Percent => {
                    let rem = builder.build_int_signed_rem(left, right, "remtmp")?;
                    rem.as_basic_value_enum()
                }
                _ => unimplemented!(),
            }
        }
        Expression::FunctionCall(expr) => {
            match expr.name.as_str() {
                "@write" => {
                    return builtin::write::handle_write_call(
                        context,
                        module,
                        builder,
                        expr,
                        symbol_table,
                    );
                }
                _ => (),
            }

            let function = module
                .get_function(&expr.name)
                .ok_or(anyhow!("unknown function `{}`", &expr.name))?;

            let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(expr.arguments.len());
            for arg_expr in &expr.arguments {
                let arg_val =
                    compile_expression_to_value(context, module, builder, arg_expr, symbol_table)?;
                let arg_meta = arg_val.into();
                args.push(arg_meta)
            }

            let call_site_value = builder.build_call(function, &args, "calltmp")?;

            match function.get_type().get_return_type() {
                Some(ret_ty) => call_site_value.try_as_basic_value().left().unwrap(),
                _ => context.i32_type().const_int(0, false).as_basic_value_enum(),
            }
        }
        expr => unimplemented!("{expr:#?}"),
    })
}
