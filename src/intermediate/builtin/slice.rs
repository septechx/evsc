use inkwell::{context::Context, types::StructType, AddressSpace};

pub fn create_slice_struct<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
    let ptr_type = context.ptr_type(AddressSpace::default());
    let len_type = context.i64_type();
    let slice_struct = context.opaque_struct_type("Slice");
    slice_struct.set_body(&[ptr_type.into(), len_type.into()], false);
    slice_struct
}
