use std::collections::HashMap;

use anyhow::Result;
use inkwell::{context::Context, AddressSpace};

use crate::{
    bindings::llvm_bindings::create_named_struct,
    intermediate::{
        arch::compile_arch_size_type,
        compiler::{CompilationContext, StructDef},
    },
};

pub fn create_slice_struct<'ctx>(
    context: &'ctx Context,
    compilation_context: &mut CompilationContext<'ctx>,
) -> Result<()> {
    let ptr_type = context.ptr_type(AddressSpace::default());
    let len_type = compile_arch_size_type(context);
    let slice_struct =
        create_named_struct(context, &[ptr_type.into(), len_type.into()], "Slice", false)?;

    compilation_context.type_context.struct_defs.insert(
        "Slice".to_string(),
        StructDef {
            llvm_type: slice_struct,
            field_indices: HashMap::from([("ptr".to_string(), 0), ("len".to_string(), 1)]),
        },
    );

    Ok(())
}
