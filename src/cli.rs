use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[clap(version, about, long_about = None, arg_required_else_help(true))]
pub struct Cli {
    #[clap(required = true)]
    pub files: Vec<PathBuf>,

    #[clap(short, long)]
    pub output: Option<PathBuf>,

    #[clap(long, help = "Emit LLVM IR")]
    pub emit_llvm: bool,
}
