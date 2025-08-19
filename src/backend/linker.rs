use anyhow::{anyhow, bail, Result};
use std::{path::Path, process::Command};

#[derive(Debug, Clone)]
pub struct LinkerOptions {
    pub output_path: String,
    pub object_files: Vec<String>,
    pub libraries: Vec<String>,
    pub static_linking: bool,
    pub strip_symbols: bool,
    pub link_libc: bool,
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
            link_libc: true,
            pie: true,
        }
    }
}

pub trait Linker {
    fn add_output(&mut self, output_path: &str);
    fn add_objects(&mut self, objects: &[String]);
    fn include_libc(&mut self);
    fn add_libraries(&mut self, libraries: &[String]);
    fn strip_symbols(&mut self);
    fn add_static(&mut self);
    fn add_shared(&mut self);
    fn add_pie(&mut self);
    fn command(&self) -> &str;
    fn args(&self) -> &[String];
}

#[derive(Debug, Clone)]
pub struct GccLinker {
    args: Vec<String>,
    command: String,
}

impl GccLinker {
    pub fn new() -> Result<Self> {
        let command = Self::find_linker()?;
        Ok(Self { args: Vec::new(), command })
    }

    fn find_linker() -> Result<String> {
        let linkers = ["gcc"]; 

        for linker in &linkers {
            if Command::new(linker).arg("--version").output().is_ok() {
                return Ok(linker.to_string());
            }
        }

        Err(anyhow!(
            "No suitable linker found. Tried: {}",
            linkers.join(", ")
        ))
    }
}

impl Linker for GccLinker {
    fn add_output(&mut self, output_path: &str) {
        self.args.push("-o".to_string());
        self.args.push(output_path.to_string());
    }

    fn add_objects(&mut self, objects: &[String]) {
        self.args.extend(objects.to_owned());
    }

    fn include_libc(&mut self) {
        // ggc always links libc
    }

    fn add_libraries(&mut self, libraries: &[String]) {
        for lib in libraries {
            self.args.push(format!("-l{lib}"));
        }
    }

    fn strip_symbols(&mut self) {
        self.args.push("-s".to_string());
    }

    fn add_static(&mut self) {
        self.args.push("-static".to_string());
    }

    fn add_shared(&mut self) {
        self.args.push("-shared".to_string());
    }

    fn add_pie(&mut self) {
        self.args.push("-pie".to_string());
    }

    fn command(&self) -> &str {
        &self.command
    }

    fn args(&self) -> &[String] {
        &self.args
    }
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

    if options.link_libc {
        linker.include_libc();
    }

    if options.pie {
        linker.add_pie();
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
    .map_err(|e| anyhow!("Failed to execute linker '{}': {}", linker.command(), e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("Linker failed:\n{}", stderr);
    }

    Ok(())
}

pub fn link_object_files(
    object_files: &[&Path],
    output_path: &Path,
    is_shared: bool,
    link_libc: bool,
    pie: bool,
) -> Result<()> {
    let object_paths: Vec<String> = object_files
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect();

    let options = LinkerOptions {
        output_path: output_path.to_string_lossy().to_string(),
        object_files: object_paths,
        libraries: Vec::new(),
        static_linking: false,
        strip_symbols: false,
        link_libc,
        pie,
    };

    let mut linker = GccLinker::new()?;

    if is_shared {
        link_shared_library(&mut linker, &options)
    } else {
        link_executable(&mut linker, &options)
    }
}
