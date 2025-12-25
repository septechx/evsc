mod asm;
mod import;
mod sizeof;
mod slice;

use anyhow::Result;
use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    ast::expressions::FunctionCallExpr,
    intermediate::{
        compiler::{CompilationContext, StructDef},
        pointer::SmartValue,
    },
};

pub trait BuiltinFunction {
    fn handle_call<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        expr: &FunctionCallExpr,
        compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<SmartValue<'ctx>>;
}

pub trait BuiltinStruct {
    fn create<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<(String, StructDef<'ctx>)>;
}

macro_rules! define_builtins {
    (
        functions {
            $( $FnVar:ident => $FnTy:path => $fn_name:expr ),* $(,)?
        }
        structs {
            $( $StVar:ident => $StTy:path => $st_name:expr ),* $(,)?
        }
    ) => {
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        pub enum Builtin {
            $( $FnVar($FnTy), )*
            $( $StVar($StTy), )*
        }

        impl Builtin {
            pub fn from_str(s: &str) -> Option<Self> {
                match s {
                    $( $fn_name => Some(Builtin::$FnVar(<$FnTy>::default())), )*
                    $( $st_name => Some(Builtin::$StVar(<$StTy>::default())), )*
                    _ => None,
                }
            }

            pub fn handle_call<'ctx>(
                &self,
                context: &'ctx Context,
                module: &Module<'ctx>,
                builder: &Builder<'ctx>,
                expr: &FunctionCallExpr,
                compilation_context: &mut CompilationContext<'ctx>,
            ) -> Result<SmartValue<'ctx>> {
                match self {
                    $( Builtin::$FnVar(_) => <$FnTy>::handle_call(context, module, builder, expr, compilation_context), )*
                    $( Builtin::$StVar(_) => panic!("`{}` is not a function builtin", $st_name), )*
                }
            }

            pub fn create<'ctx>(
                &self,
                context: &'ctx Context,
                module: &Module<'ctx>,
                builder: &Builder<'ctx>,
                compilation_context: &mut CompilationContext<'ctx>,
            ) -> Result<(String, StructDef<'ctx>)> {
                match self {
                    $( Builtin::$StVar(_) => <$StTy>::create(context, module, builder, compilation_context), )*
                    $( Builtin::$FnVar(_) => panic!("`{}` is not a struct builtin", $fn_name), )*
                }
            }
        }
    };
}

define_builtins! {
    functions {
        Asm => asm::AsmBuiltin => "asm",
        SizeOf => sizeof::SizeofBuiltin => "sizeof",
        Import => import::ImportBuiltin => "import",
    }
    structs {
        Slice => slice::SliceBuiltin => "slice",
    }
}
