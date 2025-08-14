use anyhow::{anyhow, bail, Result};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue},
    AddressSpace,
};

use crate::{
    ast::{
        ast::{Expression, Type},
        types::SliceType,
    },
    intermediate::{
        builtin,
        compile_type::compile_type,
        compiler::CompilationContext,
        import::import_module,
        pointer::{get_value, SmartValue},
    },
    lexer::token::Token,
};

pub fn compile_expression_to_value<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    expr: &Expression,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    Ok(match expr {
        Expression::Number(n) => SmartValue::from_value(
            context
                .i32_type()
                .const_int(n.value as u64, false)
                .as_basic_value_enum(),
        ),
        Expression::String(s) => {
            let string_val = context.const_string(s.value.as_bytes(), false);
            let global = module.add_global(string_val.get_type(), None, "str");
            global.set_initializer(&string_val);
            global.set_constant(true);
            global.set_linkage(Linkage::Private);

            let slice_ty = compilation_context
                .type_context
                .struct_defs
                .get("Slice")
                .unwrap()
                .llvm_type;

            let slice_val = slice_ty.get_undef();

            let ptr_val = builder.build_pointer_cast(
                global.as_pointer_value(),
                context.ptr_type(AddressSpace::default()),
                "string_ptr",
            )?;
            let len_val = context.i64_type().const_int(s.value.len() as u64, false);

            let slice_val = builder.build_insert_value(slice_val, ptr_val, 0, "slice_ptr")?;
            let slice_val = builder.build_insert_value(slice_val, len_val, 1, "slice_len")?;

            SmartValue::from_value(slice_val.as_basic_value_enum())
        }
        // Returns a pointer
        Expression::Symbol(sym) => {
            let entry = compilation_context
                .symbol_table
                .get(&sym.value)
                .cloned()
                .ok_or_else(|| anyhow!("Undefined variable `{}`", sym.value))?;

            entry.value
        }
        Expression::Prefix(expr) => {
            let right = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.right,
                compilation_context,
            )?;

            match &expr.operator {
                Token::Reference => {
                    let ptr = builder.build_alloca(right.value.get_type(), "ptr")?;
                    builder.build_store(ptr, right.value)?;

                    SmartValue::from_pointer(ptr.as_basic_value_enum(), right.value.get_type())
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

            let left_ptr = left.value.into_pointer_value();
            let right_ptr = right.value.into_pointer_value();

            let left = builder.build_load(left.pointee_ty.unwrap(), left_ptr, "left")?;
            let right = builder.build_load(right.pointee_ty.unwrap(), right_ptr, "right")?;

            match &expr.operator {
                Token::Plus => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let sum = builder.build_int_add(left, right, "sumtmp")?;
                    SmartValue::from_value(sum.as_basic_value_enum())
                }
                Token::Dash => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let sub = builder.build_int_sub(left, right, "subtmp")?;
                    SmartValue::from_value(sub.as_basic_value_enum())
                }
                Token::Star => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let mul = builder.build_int_mul(left, right, "multmp")?;
                    SmartValue::from_value(mul.as_basic_value_enum())
                }
                Token::Slash => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let div = builder.build_int_signed_div(left, right, "divtmp")?;
                    SmartValue::from_value(div.as_basic_value_enum())
                }
                Token::Percent => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let rem = builder.build_int_signed_rem(left, right, "remtmp")?;
                    SmartValue::from_value(rem.as_basic_value_enum())
                }
                _ => unimplemented!(),
            }
        }
        Expression::FunctionCall(expr) => {
            if let Expression::Symbol(sym) = expr.callee.as_ref() {
                match sym.value.as_str() {
                    "@asm" => {
                        return builtin::handle_asm_call(
                            context,
                            module,
                            builder,
                            expr,
                            compilation_context,
                        );
                    }
                    "@import" => {
                        return import_module(context, module, builder, expr, compilation_context);
                    }
                    _ => (),
                }
            }

            let callee_val = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.callee,
                compilation_context,
            )?;

            let (function_ptr, function_ty) = if let Some(pointee_ty) = callee_val.pointee_ty {
                let loaded = builder.build_load(
                    pointee_ty,
                    callee_val.value.into_pointer_value(),
                    "load_func_ptr",
                )?;
                (loaded.into_pointer_value(), pointee_ty)
            } else {
                (
                    callee_val.value.into_pointer_value(),
                    callee_val
                        .value
                        .get_type()
                        .into_pointer_type()
                        .as_basic_type_enum(),
                )
            };

            let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
            let mut arg_types: Vec<BasicMetadataTypeEnum> = Vec::new();
            for arg_expr in &expr.arguments {
                let arg_val = compile_expression_to_value(
                    context,
                    module,
                    builder,
                    arg_expr,
                    compilation_context,
                )?;
                args.push(arg_val.value.into());
                arg_types.push(arg_val.value.get_type().into());
            }

            let function_ty = function_ty.as_basic_type_enum().fn_type(&arg_types, false);

            let call_site_value =
                builder.build_indirect_call(function_ty, function_ptr, &args, "calltmp")?;

            SmartValue::from_value(
                call_site_value
                    .try_as_basic_value()
                    .left()
                    .unwrap_or(context.i32_type().const_int(0, false).as_basic_value_enum()),
            )
        }
        Expression::MemberAccess(expr) => {
            let base = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.base,
                compilation_context,
            )?;

            let base_type = get_value(builder, &base)?.get_type();

            let struct_ty = base_type.into_struct_type();
            let struct_name = struct_ty
                .get_name()
                .ok_or_else(|| anyhow!("Struct type has no name: {struct_ty:#?}"))?
                .to_str()?;
            let struct_name = if struct_name.contains(".") {
                struct_name.split(".").collect::<Vec<_>>()[0]
            } else {
                struct_name
            };
            let struct_def = compilation_context
                .type_context
                .struct_defs
                .get(struct_name)
                .ok_or_else(|| anyhow!("Unknown struct: {}", struct_name))?;

            if let Some(field_index) = struct_def.field_indices.get(&expr.member.value) {
                match base.value.get_type() {
                    BasicTypeEnum::StructType(_) => {
                        SmartValue::from_value(builder.build_extract_value(
                            base.value.into_struct_value(),
                            *field_index,
                            "field",
                        )?)
                    }
                    BasicTypeEnum::PointerType(_) => {
                        let field_ptr = builder.build_struct_gep(
                            base_type,
                            base.value.into_pointer_value(),
                            *field_index,
                            "field_ptr",
                        )?;
                        let field_ty = struct_ty.get_field_type_at_index(*field_index).unwrap();
                        SmartValue::from_pointer(field_ptr.as_basic_value_enum(), field_ty)
                    }
                    _ => bail!("Expected struct or pointer type, got {:?}", base_type),
                }
            } else if let Some(func) =
                module.get_function(&format!("{}_{}", struct_name, expr.member.value))
            {
                SmartValue::from_value(
                    func.as_global_value()
                        .as_pointer_value()
                        .as_basic_value_enum(),
                )
            } else {
                bail!("No such field or function: {}", expr.member.value);
            }
        }
        Expression::StructInstantiation(expr) => {
            let struct_def = compilation_context
                .type_context
                .struct_defs
                .get(&expr.name)
                .cloned()
                .ok_or_else(|| anyhow!("Unknown struct: {}", expr.name))?;

            let struct_ty = struct_def.llvm_type;

            // Field in instantiation but not in struct
            for field_name in expr.properties.keys() {
                if !struct_def.field_indices.contains_key(field_name) {
                    bail!("No such field {} in struct: {}", field_name, expr.name);
                }
            }
            // Field in struct but not in instantiation
            for field_name in struct_def.field_indices.keys() {
                if !expr.properties.contains_key(field_name) {
                    bail!("Missing field {} in struct {}", field_name, expr.name);
                }
            }

            let alloca = builder.build_alloca(struct_ty, &format!("inst_{}", expr.name))?;

            for (field_name, field_index) in &struct_def.field_indices {
                let expr_val = expr.properties.get(field_name).unwrap();
                let val = compile_expression_to_value(
                    context,
                    module,
                    builder,
                    expr_val,
                    compilation_context,
                )?;

                let field_ptr = builder.build_struct_gep(
                    struct_ty,
                    alloca,
                    *field_index,
                    &format!("{field_name}_ptr"),
                )?;
                builder.build_store(field_ptr, val.value)?;
            }

            let val = builder.build_load(struct_ty, alloca, "load_inst")?;

            SmartValue::from_value(val.as_basic_value_enum())
        }
        Expression::ArrayLiteral(expr) => {
            let element_ty = compile_type(context, &expr.underlying, compilation_context);
            let len = expr.contents.len();

            let array_ty = element_ty.array_type(len as u32);

            let array_ptr = builder.build_alloca(array_ty, "array")?;

            for (i, elem_expr) in expr.contents.iter().enumerate() {
                let elem_smart_val = compile_expression_to_value(
                    context,
                    module,
                    builder,
                    elem_expr,
                    compilation_context,
                )?;

                let elem_val = if let Some(pointee_ty) = elem_smart_val.pointee_ty {
                    builder.build_load(
                        pointee_ty,
                        elem_smart_val.value.into_pointer_value(),
                        "load_element",
                    )?
                } else {
                    elem_smart_val.value
                };

                let index = context.i32_type().const_int(i as u64, false);
                let elem_ptr = unsafe {
                    builder.build_gep(
                        array_ty,
                        array_ptr,
                        &[context.i32_type().const_int(0, false), index],
                        "elem_ptr",
                    )?
                };

                builder.build_store(elem_ptr, elem_val)?;
            }

            let base_ptr = unsafe {
                builder.build_gep(
                    array_ty,
                    array_ptr,
                    &[
                        context.i32_type().const_int(0, false),
                        context.i32_type().const_int(0, false),
                    ],
                    "first_elem_ptr",
                )?
            };

            let slice_ty = Type::Slice(SliceType {
                underlying: Box::new(expr.underlying.clone()),
            });

            let slice_llvm_type = compile_type(context, &slice_ty, compilation_context);

            let slice_struct = match slice_llvm_type {
                BasicTypeEnum::StructType(ty) => ty,
                _ => bail!("Slice type did not compile to a struct"),
            };

            let len_val = context.i64_type().const_int(len as u64, false);

            let slice_val = slice_struct.get_undef();

            let slice_val = builder.build_insert_value(slice_val, base_ptr, 0, "slice_ptr")?;
            let slice_val = builder.build_insert_value(slice_val, len_val, 1, "slice_len")?;

            SmartValue::from_value(slice_val.as_basic_value_enum())
        }
        expr => unimplemented!("{expr:#?}"),
    })
}
