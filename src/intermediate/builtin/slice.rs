use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum, StructType},
    AddressSpace,
};

pub fn create_slice_struct<'ctx>(
    context: &'ctx Context,
    element_ty: BasicTypeEnum<'ctx>,
) -> StructType<'ctx> {
    let ptr_type = element_ty.ptr_type(AddressSpace::default());
    let len_type = context.i64_type();
    context.struct_type(&[ptr_type.into(), len_type.into()], false)
}
