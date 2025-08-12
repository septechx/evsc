use inkwell::{builder::Builder, types::BasicTypeEnum, values::BasicValueEnum};

#[derive(Clone, Debug)]
pub struct SmartValue<'ctx> {
    pub value: BasicValueEnum<'ctx>,
    pub pointee_ty: Option<BasicTypeEnum<'ctx>>,
}

impl<'ctx> SmartValue<'ctx> {
    pub fn from_value(value: BasicValueEnum<'ctx>) -> Self {
        Self {
            value,
            pointee_ty: None,
        }
    }

    pub fn from_pointer(value: BasicValueEnum<'ctx>, pointee_ty: BasicTypeEnum<'ctx>) -> Self {
        Self {
            value,
            pointee_ty: Some(pointee_ty),
        }
    }
}

pub fn get_value<'ctx>(
    builder: &Builder<'ctx>,
    ptr: &SmartValue<'ctx>,
) -> anyhow::Result<BasicValueEnum<'ctx>> {
    Ok(if let Some(pointee_ty) = ptr.pointee_ty {
        let ret_ptr = ptr.value.into_pointer_value();
        builder.build_load(pointee_ty, ret_ptr, "load_ptr")?
    } else {
        ptr.value
    })
}
