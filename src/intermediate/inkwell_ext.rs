use std::ffi::CString;

use anyhow::Result;
use inkwell::{
    llvm_sys::core::{LLVMAddGlobal, LLVMSetGlobalConstant, LLVMSetInitializer},
    module::Module,
    types::BasicType,
    values::{BasicValue, GlobalValue},
};

pub fn add_global_constant<'ctx, T: BasicType<'ctx>, V: BasicValue<'ctx>>(
    module: &Module<'ctx>,
    type_: T,
    name: &str,
    initializer: V,
) -> Result<GlobalValue<'ctx>> {
    let c_name = CString::new(name)?;

    let gv = unsafe {
        let g = LLVMAddGlobal(module.as_mut_ptr(), type_.as_type_ref(), c_name.as_ptr());
        LLVMSetInitializer(g, initializer.as_value_ref());
        LLVMSetGlobalConstant(g, 1);
        g
    };

    Ok(unsafe { GlobalValue::new(gv) })
}
