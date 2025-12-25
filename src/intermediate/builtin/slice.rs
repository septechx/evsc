use std::collections::HashMap;

use anyhow::Result;
use inkwell::{AddressSpace, context::Context};

use crate::{
    bindings::llvm_bindings::create_named_struct,
    intermediate::{
        arch::compile_arch_size_type,
        builtin::BuiltinStruct,
        compiler::{CompilationContext, StructDef},
    },
    struct_fields,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct SliceBuiltin;

impl BuiltinStruct for SliceBuiltin {
    fn create<'ctx>(
        context: &'ctx Context,
        _compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<StructDef<'ctx>> {
        let ptr_type = context.ptr_type(AddressSpace::default());
        let len_type = compile_arch_size_type(context);
        let slice_struct =
            create_named_struct(context, &[ptr_type.into(), len_type.into()], "Slice", false)?;

        Ok(StructDef {
            llvm_type: slice_struct,
            field_indices: struct_fields!(ptr, len),
            is_builtin: true,
        })
    }
}
