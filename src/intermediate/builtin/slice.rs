use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum, StructType},
    AddressSpace,
};

pub fn create_slice_struct<'ctx>(
    context: &'ctx Context,
    element_ty: BasicTypeEnum<'ctx>,
    name: &str,
) -> StructType<'ctx> {
    let ptr_type = element_ty.ptr_type(AddressSpace::default());
    let len_type = context.i64_type();
    let slice_struct = context.opaque_struct_type(name);
    slice_struct.set_body(&[ptr_type.into(), len_type.into()], false);
    slice_struct
}
