mod gcc;
mod ld;

pub mod linkers {
    pub use super::gcc::GccLinker;
    pub use super::ld::LdLinker;
}

use anyhow::{Result, anyhow};
use std::{path::Path, process::Command};

use crate::errors::builders;

#[derive(Debug, Clone)]
pub struct LinkerOptions {
    pub output_path: String,
    pub object_files: Vec<String>,
    pub libraries: Vec<String>,
    pub static_linking: bool,
    pub strip_symbols: bool,
    pub pie: bool,
}

impl Default for LinkerOptions {
    fn default() -> Self {
        Self {
            output_path: "a.out".to_string(),
            object_files: Vec::new(),
            libraries: Vec::new(),
            static_linking: false,
            strip_symbols: false,
            pie: true,
        }
    }
}

pub trait Linker {
    fn new() -> Self;
    fn add_output(&mut self, output_path: &str);
    fn add_objects(&mut self, objects: &[String]);
    fn add_libraries(&mut self, libraries: &[String]);
    fn strip_symbols(&mut self);
    fn add_static(&mut self);
    fn add_shared(&mut self);
    fn add_pie(&mut self);
    fn add_no_pie(&mut self);
    fn add_dynamic_linker(&mut self);
    fn command(&self) -> &str;
    fn args(&self) -> &[String];
}

pub fn link_executable(linker: &mut impl Linker, options: &LinkerOptions) -> Result<()> {
    linker.add_output(&options.output_path);
    linker.add_objects(&options.object_files);
    linker.add_libraries(&options.libraries);

    if options.strip_symbols {
        linker.strip_symbols();
    }

    if options.static_linking {
        linker.add_static();
    }

    if options.pie {
        linker.add_pie();
    } else {
        linker.add_no_pie();
    }

    if options.pie && !options.static_linking {
        linker.add_dynamic_linker();
    }

    run_linker(linker)
}

pub fn link_shared_library(linker: &mut impl Linker, options: &LinkerOptions) -> Result<()> {
    linker.add_output(&options.output_path);
    linker.add_objects(&options.object_files);
    linker.add_libraries(&options.libraries);
    linker.add_shared();

    if options.strip_symbols {
        linker.strip_symbols();
    }

    run_linker(linker)
}

fn run_linker(linker: &impl Linker) -> Result<()> {
    let output = Command::new(linker.command())
        .args(linker.args())
        .output()
        .map_err(|e| anyhow!("{}: {}", linker.command(), e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        crate::ERRORS.with(|e| {
            e.collector.borrow_mut().add(builders::fatal(stderr));
        });
    }

    Ok(())
}

pub fn link_object_files<T: Linker>(
    object_files: &[&Path],
    output_path: &Path,
    is_shared: bool,
    pie: bool,
    static_linking: bool,
) -> Result<()> {
    let object_paths: Vec<String> = object_files
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect();

    let options = LinkerOptions {
        output_path: output_path.to_string_lossy().to_string(),
        object_files: object_paths,
        libraries: Vec::new(),
        static_linking,
        strip_symbols: false,
        pie,
    };

    let mut linker = T::new();

    if is_shared {
        link_shared_library(&mut linker, &options)
    } else {
        link_executable(&mut linker, &options)
    }
}
