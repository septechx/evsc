use anyhow::Result;
use inkwell::{
    context::{AsContextRef, Context},
    llvm_sys::prelude::*,
    types::{AsTypeRef, BasicTypeEnum, StructType},
};
use std::{
    ffi::CString,
    os::raw::{c_char, c_uint},
};

#[link(name = "llvm_bindings")]
unsafe extern "C" {
    fn LLVMExtCreateNamedStruct(
        ctx: LLVMContextRef,
        elements: *mut LLVMTypeRef,
        count: c_uint,
        name: *const c_char,
        isPacked: bool,
    ) -> LLVMTypeRef;
}

pub fn create_named_struct<'ctx>(
    context: &'ctx Context,
    elements: &[BasicTypeEnum<'ctx>],
    name: &str,
    is_packed: bool,
) -> Result<StructType<'ctx>> {
    let name = CString::new(name)?;

    let elements = elements
        .iter()
        .map(|ty| ty.as_type_ref())
        .collect::<Vec<_>>();

    let struct_ty = unsafe {
        LLVMExtCreateNamedStruct(
            context.as_ctx_ref(),
            elements.as_ptr() as *mut _,
            elements.len() as u32,
            name.as_ptr(),
            is_packed,
        )
    };

    Ok(unsafe { StructType::new(struct_ty) })
}
