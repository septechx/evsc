use anyhow::{anyhow, bail, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
    AddressSpace,
};

use crate::{
    ast::{
        ast::{Expression, Type},
        types::{SliceType, SymbolType},
    },
    intermediate::{
        builtin::{self, slice::create_slice_struct},
        compile_type::compile_type,
        compiler::CompilationContext,
    },
    lexer::token::TokenKind,
};

pub fn compile_expression_to_value<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    expr: &Expression,
    compilation_context: &mut CompilationContext<'ctx>,
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
            global.set_linkage(Linkage::Private);

            let u8_type = Type::Symbol(SymbolType {
                name: "u8".to_string(),
            });
            let slice_type = Type::Slice(SliceType {
                underlying: Box::new(u8_type),
            });

            let slice_llvm_type = compile_type(context, &slice_type, compilation_context);
            let slice_struct = match slice_llvm_type {
                BasicTypeEnum::StructType(ty) => ty,
                _ => bail!("Slice type did not compile to a struct"),
            };

            let slice_val = slice_struct.get_undef();

            let ptr_val = builder.build_pointer_cast(
                global.as_pointer_value(),
                context.i8_type().ptr_type(AddressSpace::default()),
                "string_ptr",
            )?;
            let len_val = context.i64_type().const_int(s.value.len() as u64, false);

            let slice_val = builder.build_insert_value(slice_val, ptr_val, 0, "slice_ptr")?;
            let slice_val = builder.build_insert_value(slice_val, len_val, 1, "slice_len")?;

            slice_val.as_basic_value_enum()
        }
        Expression::Symbol(sym) => compilation_context
            .symbol_table
            .get(&sym.value)
            .cloned()
            .ok_or_else(|| anyhow!("Undefined variable `{}`", sym.value))?,
        Expression::Prefix(expr) => {
            let right = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.right,
                compilation_context,
            )?;

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
            let left = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.left,
                compilation_context,
            )?;
            let right = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.right,
                compilation_context,
            )?;

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
                "@asm" => {
                    return builtin::asm::handle_asm_call(
                        context,
                        module,
                        builder,
                        expr,
                        compilation_context,
                    );
                }
                _ => (),
            }

            let function = module
                .get_function(&expr.name)
                .ok_or(anyhow!("unknown function `{}`", &expr.name))?;

            let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(expr.arguments.len());
            for arg_expr in &expr.arguments {
                let arg_val = compile_expression_to_value(
                    context,
                    module,
                    builder,
                    arg_expr,
                    compilation_context,
                )?;
                let arg_meta = arg_val.into();
                args.push(arg_meta)
            }

            let call_site_value = builder.build_call(function, &args, "calltmp")?;

            match function.get_type().get_return_type() {
                Some(_) => call_site_value.try_as_basic_value().left().unwrap(),
                _ => context.i32_type().const_int(0, false).as_basic_value_enum(),
            }
        }
        Expression::MemberAccess(expr) => {
            let base = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.base,
                compilation_context,
            )?;
            let base_type = base.get_type();

            if base_type.is_struct_type() {
                let struct_ty = base_type.into_struct_type();
                let struct_name = struct_ty
                    .get_name()
                    .ok_or_else(|| anyhow!("Struct type has no name: {struct_ty:#?}"))?
                    .to_str()?;
                let struct_def = compilation_context
                    .type_context
                    .struct_defs
                    .get(struct_name)
                    .ok_or_else(|| anyhow!("Unknown struct: {}", struct_name))?;
                let field_index = struct_def
                    .field_indices
                    .get(&expr.member.value)
                    .ok_or_else(|| anyhow!("No such field: {}", &expr.member.value))?;

                builder.build_extract_value(base.into_struct_value(), *field_index, "field")?
            } else {
                bail!("Member access on non-struct type");
            }
        }
        expr => unimplemented!("{expr:#?}"),
    })
}
