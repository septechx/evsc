use anyhow::{anyhow, Result};
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

pub fn generate_common_args(options: &LinkerOptions) -> Vec<String> {
    let mut args = Vec::new();

    args.push("-o".to_string());
    args.push(options.output_path.clone());
    args.extend(options.object_files.clone());

    if options.link_libc {
        args.push("-lc".to_string());
    }

    for lib in &options.libraries {
        args.push(format!("-l{lib}"));
    }

    if options.strip_symbols {
        args.push("-s".to_string());
    }

    args
}

pub fn link_executable(options: &LinkerOptions) -> Result<()> {
    let linker = find_linker()?;

    let mut args = Vec::new();

    args.extend(generate_common_args(options));

    if options.static_linking {
        args.push("-static".to_string());
    }

    let output = Command::new(&linker)
        .args(&args)
        .output()
        .map_err(|e| anyhow!("Failed to execute linker '{}': {}", linker, e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("Linker failed:\n{}", stderr));
    }

    Ok(())
}

pub fn link_shared_library(lib_options: &LinkerOptions) -> Result<()> {
    let linker = find_linker()?;

    let mut args = Vec::new();

    args.push("--shared".to_string());

    args.extend(generate_common_args(lib_options));

    let output = Command::new(&linker)
        .args(&args)
        .output()
        .map_err(|e| anyhow!("Failed to execute linker '{}': {}", linker, e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("Linker failed:\n{}", stderr));
    }

    Ok(())
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

    if is_shared {
        link_shared_library(&options)
    } else {
        link_executable(&options)
    }
}
