use std::{path::PathBuf, str::FromStr};

use anyhow::{anyhow, Error, Result};
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
            "0" | "O0" | "o0" => Ok(OptLevel::O0),
            "1" | "O1" | "o1" => Ok(OptLevel::O1),
            "2" | "O2" | "o2" => Ok(OptLevel::O2),
            "3" | "O3" | "o3" => Ok(OptLevel::O3),
            other => Err(anyhow!("invalid optimization level: {}", other)),
        }
    }
}

impl Into<OptimizationLevel> for OptLevel {
    fn into(self) -> OptimizationLevel {
        match self {
            OptLevel::O0 => OptimizationLevel::None,
            OptLevel::O1 => OptimizationLevel::Less,
            OptLevel::O2 => OptimizationLevel::Default,
            OptLevel::O3 => OptimizationLevel::Aggressive,
        }
    }
}

#[derive(Parser, Debug)]
#[clap(version, about, long_about = None, arg_required_else_help(true))]
pub struct Cli {
    #[clap(required = true)]
    pub files: Vec<PathBuf>,

    #[clap(short, long)]
    pub output: Option<PathBuf>,

    #[clap(long, help = "Emit LLVM IR")]
    pub emit_llvm: bool,

    #[clap(long, help = "Emit assembly")]
    pub emit_asm: bool,

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

    #[clap(long = "fPIC", help = "Generate position independent code")]
    pub pic: bool,

    #[clap(long = "shared", help = "Generate a shared library")]
    pub shared: bool,

    #[clap(long = "static", help = "Generate a static library")]
    pub static_: bool,
}
