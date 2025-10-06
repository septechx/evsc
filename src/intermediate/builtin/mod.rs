mod asm;
mod sizeof;
mod slice;

pub use asm::handle_asm_call;
pub use sizeof::handle_sizeof_call;
pub use slice::create_slice_struct;
