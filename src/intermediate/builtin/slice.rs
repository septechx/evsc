use std::collections::HashMap;

use inkwell::{context::Context, AddressSpace};

use crate::intermediate::compiler::{CompilationContext, StructDef};

pub fn create_slice_struct<'ctx>(
    context: &'ctx Context,
    compilation_context: &mut CompilationContext<'ctx>,
) {
    let ptr_type = context.ptr_type(AddressSpace::default());
    let len_type = context.i64_type();
    let slice_struct = context.opaque_struct_type("Slice");
    slice_struct.set_body(&[ptr_type.into(), len_type.into()], false);

    compilation_context.type_context.struct_defs.insert(
        "Slice".to_string(),
        StructDef {
            llvm_type: slice_struct,
            field_indices: HashMap::from([("ptr".to_string(), 0), ("len".to_string(), 1)]),
        },
    );
}
