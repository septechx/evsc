use std::{path::PathBuf, str::FromStr};

use anyhow::{Error, Result, anyhow};
use clap::Parser;
use inkwell::OptimizationLevel;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum OptLevel {
    O0,
    O1,
    O2,
    O3,
}

impl FromStr for OptLevel {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(OptLevel::O0),
            "1" => Ok(OptLevel::O1),
            "2" => Ok(OptLevel::O2),
            "3" => Ok(OptLevel::O3),
            other => Err(anyhow!("invalid optimization level: {}", other)),
        }
    }
}

impl From<OptLevel> for OptimizationLevel {
    fn from(level: OptLevel) -> OptimizationLevel {
        match level {
            OptLevel::O0 => OptimizationLevel::None,
            OptLevel::O1 => OptimizationLevel::Less,
            OptLevel::O2 => OptimizationLevel::Default,
            OptLevel::O3 => OptimizationLevel::Aggressive,
        }
    }
}

#[derive(Debug, Clone, Copy, clap::ValueEnum, Default)]
pub enum ColorChoice {
    #[default]
    Auto,
    Always,
    Never,
}

#[derive(Parser, Debug)]
#[clap(version, about, long_about = None, arg_required_else_help(true))]
pub struct Cli {
    #[clap(required = true)]
    pub input: Vec<PathBuf>,

    #[clap(short, long)]
    pub output: Option<PathBuf>,

    #[clap(long, help = "Emit LLVM IR")]
    pub emit_llvm: bool,

    #[clap(long, help = "Emit assembly")]
    pub emit_asm: bool,

    #[clap(long, help = "Print AST")]
    pub print_ast: bool,

    #[clap(
        long,
        value_enum,
        default_value_t = ColorChoice::Auto,
        help = "When to use colors: auto, always, never. Default: auto"
    )]
    pub color: ColorChoice,

    #[clap(long, help = "Do not print any output")]
    pub quiet: bool,

    #[clap(
        long = "Dcpu",
        help = "Select a CPU architecture to target. Default: x86-64"
    )]
    pub cpu: Option<String>,

    #[clap(
        long = "Dfeatures",
        help = "Select a feature set to enable. Default: +avx2"
    )]
    pub features: Option<String>,

    #[clap(short = 'O', help = "Set optimization level. Default: 3")]
    pub opt: Option<OptLevel>,

    #[clap(long = "no-pie", help = "Disable position independent executable")]
    pub no_pie: bool,

    #[clap(long = "no-pic", help = "Disable position independent code")]
    pub no_pic: bool,

    #[clap(long = "shared", help = "Generate a shared library")]
    pub shared: bool,

    #[clap(long = "static", help = "Generate a static library")]
    pub static_: bool,

    #[clap(long = "no-link", help = "Only compile, do not link")]
    pub no_link: bool,

    #[clap(long = "strip", help = "Strip symbols from executable")]
    pub strip: bool,

    #[clap(long = "gcc", help = "Use gcc as a linker instead of mold/lld/gold/ld")]
    pub use_gcc: bool,
}
