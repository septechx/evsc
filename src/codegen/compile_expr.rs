use anyhow::{Result, anyhow, bail};
use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue},
};

use crate::{
    ast::{Expr, ExprKind, Literal, Type, TypeKind, expressions::ReturnExpr, types::SliceType},
    codegen::{
        arch::compile_arch_size_type,
        builtin::{Builtin, get_builtin},
        compile_type::{cast_int_to_type, compile_type},
        compiler::{CompilationContext, compile_stmts},
        pointer::SmartValue,
    },
    hashmap::FxHashMap,
    lexer::token::TokenKind,
    span::Span,
};

pub fn compile_expression_to_value<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    expr: &Expr,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<SmartValue<'ctx>> {
    Ok(match &expr.kind {
        ExprKind::Literal(lit) => match lit {
            Literal::Integer(i) => SmartValue::from_value(
                compile_arch_size_type(context)
                    .const_int(*i as u64, false)
                    .as_basic_value_enum(),
            ),
            Literal::Float(f) => {
                SmartValue::from_value(context.f64_type().const_float(*f).as_basic_value_enum())
            }
            Literal::Bool(b) => SmartValue::from_value(
                context
                    .bool_type()
                    .const_int(*b as u64, false)
                    .as_basic_value_enum(),
            ),
            Literal::Char(c) => SmartValue::from_value(
                context
                    .i8_type()
                    .const_int(*c as u64, false)
                    .as_basic_value_enum(),
            ),
            Literal::String(s) => {
                let string_val = context.const_string(s.as_bytes(), false);
                let global = module.add_global(string_val.get_type(), None, "str");
                global.set_initializer(&string_val);
                global.set_constant(true);
                global.set_linkage(Linkage::Private);

                let slice_ty = get_builtin(context, compilation_context, Builtin::Slice)?.llvm_type;

                let slice_val = slice_ty.get_undef();

                let ptr_val = builder.build_pointer_cast(
                    global.as_pointer_value(),
                    context.ptr_type(AddressSpace::default()),
                    "string_ptr",
                )?;
                let len_val = compile_arch_size_type(context).const_int(s.len() as u64, false);

                let slice_val = builder.build_insert_value(slice_val, ptr_val, 0, "slice_ptr")?;
                let slice_val = builder.build_insert_value(slice_val, len_val, 1, "slice_len")?;

                SmartValue::from_value(slice_val.as_basic_value_enum())
            }
        },
        // Returns a pointer
        ExprKind::Symbol(sym) => {
            let entry = compilation_context
                .symbol_table
                .get(sym.value.value.as_ref())
                .cloned()
                .ok_or_else(|| anyhow!("Undefined variable `{}`", sym.value.value))?;

            if let Some(fn_entry) = compilation_context
                .function_table
                .get(sym.value.value.as_ref())
            {
                entry.value.with_fn_type(fn_entry.function.get_type())
            } else {
                entry.value
            }
        }
        ExprKind::Prefix(expr) => {
            let right = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.right,
                compilation_context,
            )?;

            match expr.operator.kind {
                TokenKind::Reference => {
                    let ptr = builder.build_alloca(right.value.get_type(), "ptr")?;
                    builder.build_store(ptr, right.value)?;

                    SmartValue::from_pointer(ptr.as_basic_value_enum(), right.value.get_type())
                }
                TokenKind::Dash => {
                    let right_val = right.unwrap(builder)?.into_int_value();
                    let neg = builder.build_int_neg(right_val, "negtmp")?;
                    SmartValue::from_value(neg.as_basic_value_enum())
                }
                _ => unimplemented!(),
            }
        }
        ExprKind::Binary(expr) => {
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

            let left = left.unwrap(builder)?;
            let right = right.unwrap(builder)?;

            match expr.operator.kind {
                TokenKind::Plus => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let sum = builder.build_int_add(left, right, "sumtmp")?;
                    SmartValue::from_value(sum.as_basic_value_enum())
                }
                TokenKind::Dash => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let sub = builder.build_int_sub(left, right, "subtmp")?;
                    SmartValue::from_value(sub.as_basic_value_enum())
                }
                TokenKind::Star => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let mul = builder.build_int_mul(left, right, "multmp")?;
                    SmartValue::from_value(mul.as_basic_value_enum())
                }
                TokenKind::Slash => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let div = builder.build_int_signed_div(left, right, "divtmp")?;
                    SmartValue::from_value(div.as_basic_value_enum())
                }
                TokenKind::Percent => {
                    let left = left.into_int_value();
                    let right = right.into_int_value();
                    let rem = builder.build_int_signed_rem(left, right, "remtmp")?;
                    SmartValue::from_value(rem.as_basic_value_enum())
                }
                _ => unimplemented!(),
            }
        }
        ExprKind::FunctionCall(fn_expr) => {
            if let ExprKind::Symbol(sym) = &fn_expr.callee.kind
                && let Some(builtin) = sym.value.value.strip_prefix("@")
                && let Some(builtin) = Builtin::from_str(builtin)
            {
                return builtin.handle_call(context, module, builder, expr, compilation_context);
            }

            let callee_val = compile_expression_to_value(
                context,
                module,
                builder,
                &fn_expr.callee,
                compilation_context,
            )?;

            let (function_ptr, function_ty) = if let Some(pointee_ty) = callee_val.pointee_ty {
                (callee_val.value.into_pointer_value(), pointee_ty)
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

            let fn_type = callee_val.fn_type;

            let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
            let mut arg_types: Vec<BasicMetadataTypeEnum> = Vec::new();
            for (i, arg_expr) in fn_expr.parameters.iter().enumerate() {
                let arg_val = compile_expression_to_value(
                    context,
                    module,
                    builder,
                    arg_expr,
                    compilation_context,
                )?;
                let mut loaded_val = arg_val.unwrap(builder)?;

                if let Some(ft) = fn_type
                    && let Some(param_type) = ft.get_param_types().get(i)
                    && let Ok(basic_type) = (*param_type).try_into()
                {
                    loaded_val = cast_int_to_type(builder, loaded_val, basic_type)?;
                }

                args.push(loaded_val.into());
                arg_types.push(loaded_val.get_type().into());
            }

            let function_ty = if let Some(ft) = fn_type {
                ft
            } else {
                function_ty.as_basic_type_enum().fn_type(&arg_types, false)
            };

            let call_site_value =
                builder.build_indirect_call(function_ty, function_ptr, &args, "calltmp")?;

            SmartValue::from_value(
                call_site_value
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| anyhow!("Espected call site value to be a basic value"))?,
            )
        }
        ExprKind::MemberAccess(expr) => {
            let base = compile_expression_to_value(
                context,
                module,
                builder,
                &expr.base,
                compilation_context,
            )?;

            let base_type = base.pointee_ty.unwrap_or_else(|| base.value.get_type());

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

            if let Some(field_index) = struct_def.field_indices.get(expr.member.value.as_ref()) {
                match base.value.get_type() {
                    BasicTypeEnum::StructType(_) => {
                        SmartValue::from_value(builder.build_extract_value(
                            base.value.into_struct_value(),
                            *field_index,
                            "field",
                        )?)
                    }
                    BasicTypeEnum::PointerType(_) => {
                        let loaded_struct = builder.build_load(
                            struct_ty,
                            base.value.into_pointer_value(),
                            "loaded_struct",
                        )?;
                        let field_value = builder.build_extract_value(
                            loaded_struct.into_struct_value(),
                            *field_index,
                            "field",
                        )?;
                        SmartValue::from_value(field_value)
                    }
                    _ => bail!("Expected struct or pointer type, got {:?}", base_type),
                }
            } else if let Some(func) =
                module.get_function(&format!("{}_{}", struct_name, expr.member.value))
            {
                SmartValue::from_pointer(
                    func.as_global_value().as_basic_value_enum(),
                    func.get_type()
                        .get_return_type()
                        .expect("function isn't void"),
                )
                .with_fn_type(func.get_type())
            } else {
                bail!("No such field or function: {}", expr.member.value);
            }
        }
        ExprKind::StructInstantiation(expr) => {
            let struct_def = compilation_context
                .type_context
                .struct_defs
                .get(expr.name.value.as_ref())
                .cloned()
                .ok_or_else(|| anyhow!("Unknown struct: {}", expr.name.value))?;

            let struct_ty = struct_def.llvm_type;

            let properties = expr
                .fields
                .iter()
                .map(|(ident, expr)| (ident.value.clone(), expr))
                .collect::<FxHashMap<_, _>>();

            // Field in instantiation but not in struct
            for field_name in expr.fields.keys() {
                if !struct_def
                    .field_indices
                    .contains_key(field_name.value.as_ref())
                {
                    bail!(
                        "No such field {} in struct: {}",
                        field_name.value,
                        expr.name.value
                    );
                }
            }
            // Field in struct but not in instantiation
            for field_name in struct_def.field_indices.keys() {
                if !properties.contains_key(field_name.as_ref()) {
                    bail!("Missing field {} in struct {}", field_name, expr.name.value);
                }
            }

            let alloca = builder.build_alloca(struct_ty, &format!("inst_{}", expr.name.value))?;

            for (field_name, field_index) in &struct_def.field_indices {
                let expr_val = properties.get(field_name.as_ref()).expect("field exists");
                let val = compile_expression_to_value(
                    context,
                    module,
                    builder,
                    expr_val,
                    compilation_context,
                )?;

                let val = val.unwrap(builder)?;

                let field_ptr = builder.build_struct_gep(
                    struct_ty,
                    alloca,
                    *field_index,
                    &format!("{field_name}_ptr"),
                )?;

                let field_type = struct_ty
                    .get_field_type_at_index(*field_index)
                    .expect("field exists");
                let store_val = cast_int_to_type(builder, val, field_type)?;

                builder.build_store(field_ptr, store_val)?;
            }

            let val = builder.build_load(struct_ty, alloca, "load_inst")?;

            SmartValue::from_value(val.as_basic_value_enum())
        }
        ExprKind::ArrayLiteral(ar_expr) => {
            let element_ty = compile_type(context, &ar_expr.underlying, compilation_context)?;
            let len = ar_expr.contents.len();

            let array_ty = element_ty.array_type(len as u32);

            let array_ptr = builder.build_alloca(array_ty, "array")?;

            for (i, elem_expr) in ar_expr.contents.iter().enumerate() {
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

                let index = compile_arch_size_type(context).const_int(i as u64, false);
                let elem_ptr = unsafe {
                    builder.build_gep(
                        array_ty,
                        array_ptr,
                        &[compile_arch_size_type(context).const_int(0, false), index],
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
                        compile_arch_size_type(context).const_int(0, false),
                        compile_arch_size_type(context).const_int(0, false),
                    ],
                    "first_elem_ptr",
                )?
            };

            let slice_ty = Type {
                kind: TypeKind::Slice(SliceType {
                    underlying: Box::new(ar_expr.underlying.clone()),
                }),
                span: Span::new(expr.span.start(), ar_expr.underlying.span.end()),
            };

            let slice_llvm_type = compile_type(context, &slice_ty, compilation_context)?;

            let slice_struct = match slice_llvm_type {
                BasicTypeEnum::StructType(ty) => ty,
                _ => bail!("Slice type did not compile to a struct"),
            };

            let len_val = compile_arch_size_type(context).const_int(len as u64, false);

            let slice_val = slice_struct.get_undef();

            let slice_val = builder.build_insert_value(slice_val, base_ptr, 0, "slice_ptr")?;
            let slice_val = builder.build_insert_value(slice_val, len_val, 1, "slice_len")?;

            SmartValue::from_value(slice_val.as_basic_value_enum())
        }
        ExprKind::Return(ret) => {
            compile_return(context, module, builder, ret, compilation_context)?;
            SmartValue::from_value(
                compile_arch_size_type(context)
                    .const_int(0, false)
                    .as_basic_value_enum(),
            )
        }
        // TODO: Return the value of the last expression in the block
        ExprKind::Block(block) => {
            let mut inner_compilation_context = compilation_context.clone();
            compile_stmts(
                context,
                module,
                builder,
                &block.block.stmts,
                &mut inner_compilation_context,
            )?;
            SmartValue::from_value(
                compile_arch_size_type(context)
                    .const_int(0, false)
                    .as_basic_value_enum(),
            )
        }
        expr => unimplemented!("{expr:#?}"),
    })
}

fn compile_return<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    ret: &ReturnExpr,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    if let Some(expr) = &ret.value {
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
            bail!("cannot return a value from a void function");
        }
    } else {
        let function = builder
            .get_insert_block()
            .expect("function has a block")
            .get_parent()
            .expect("block has a function");
        let expected_ret_type = function.get_type().get_return_type();

        if expected_ret_type.is_some() {
            bail!("cannot return a value from a void function");
        } else {
            builder.build_return(None)?;
        }
    }

    Ok(())
}
