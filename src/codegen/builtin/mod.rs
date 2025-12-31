mod asm;
mod import;
mod sizeof;
mod slice;

use anyhow::Result;
use inkwell::{builder::Builder, context::Context, module::Module};

use crate::{
    ast::Expr,
    codegen::{
        compiler::{CompilationContext, StructDef},
        pointer::SmartValue,
    },
};

pub trait BuiltinFunction {
    fn handle_call<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
        expr: &Expr,
        compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<SmartValue<'ctx>>;
}

pub trait BuiltinStruct {
    fn create<'ctx>(
        context: &'ctx Context,
        compilation_context: &mut CompilationContext<'ctx>,
    ) -> Result<StructDef<'ctx>>;
}

pub fn get_builtin<'ctx>(
    context: &'ctx Context,
    compilation_context: &mut CompilationContext<'ctx>,
    builtin: Builtin,
) -> Result<StructDef<'ctx>> {
    if let Some(found) = compilation_context
        .type_context
        .struct_defs
        .get(builtin.name())
    {
        Ok(found.clone())
    } else {
        compilation_context.builtins.insert(builtin);
        let struct_def = builtin.create(context, compilation_context)?;
        compilation_context
            .type_context
            .struct_defs
            .insert(builtin.name().into(), struct_def.clone());
        Ok(struct_def)
    }
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
        #[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
        pub enum Builtin {
            $( $FnVar, )*
            $( $StVar, )*
        }

        impl Builtin {
            pub fn from_str(s: &str) -> Option<Self> {
                match s {
                    $( $fn_name => Some(Builtin::$FnVar), )*
                    $( $st_name => Some(Builtin::$StVar), )*
                    _ => None,
                }
            }

            pub fn name(&self) -> &'static str {
                match self {
                    $( Builtin::$FnVar => $fn_name, )*
                    $( Builtin::$StVar => $st_name, )*
                }
            }

            pub fn handle_call<'ctx>(
                &self,
                context: &'ctx Context,
                module: &Module<'ctx>,
                builder: &Builder<'ctx>,
                expr: &Expr,
                compilation_context: &mut CompilationContext<'ctx>,
            ) -> Result<SmartValue<'ctx>> {
                match self {
                    $( Builtin::$FnVar => <$FnTy>::handle_call(context, module, builder, expr, compilation_context), )*
                    $( Builtin::$StVar => panic!("`{}` is not a function builtin", $st_name), )*
                }
            }

            pub fn create<'ctx>(
                &self,
                context: &'ctx Context,
                compilation_context: &mut CompilationContext<'ctx>,
            ) -> Result<StructDef<'ctx>> {
                match self {
                    $( Builtin::$StVar => <$StTy>::create(context, compilation_context), )*
                    $( Builtin::$FnVar => panic!("`{}` is not a struct builtin", $fn_name), )*
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
        Slice => slice::SliceBuiltin => "Slice",
    }
}
