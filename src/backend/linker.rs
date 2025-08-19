use anyhow::{anyhow, Result};
use std::{path::Path, process::Command};

#[derive(Debug, Clone)]
pub struct LinkerOptions {
    pub output_path: String,
    pub object_files: Vec<String>,
    pub libraries: Vec<String>,
    pub static_linking: bool,
    pub strip_symbols: bool,
    pub verbose: bool,
    pub link_libc: bool,
}

impl Default for LinkerOptions {
    fn default() -> Self {
        Self {
            output_path: "a.out".to_string(),
            object_files: Vec::new(),
            libraries: Vec::new(),
            static_linking: false,
            strip_symbols: false,
            verbose: false,
            link_libc: true,
        }
    }
}

pub fn link_executable(options: &LinkerOptions) -> Result<()> {
    let linker = find_linker()?;

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

    if options.static_linking {
        args.push("-static".to_string());
    }

    if options.strip_symbols {
        args.push("-s".to_string());
    }

    if options.verbose {
        args.push("-v".to_string());
    }

    if options.verbose {
        eprintln!("Running linker: {linker} {args:?}");
    }

    let output = Command::new(&linker)
        .args(&args)
        .output()
        .map_err(|e| anyhow!("Failed to execute linker '{}': {}", linker, e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("Linker failed:\n{}", stderr));
    }

    if options.verbose {
        let stdout = String::from_utf8_lossy(&output.stdout);
        if !stdout.is_empty() {
            eprintln!("Linker output:\n{stdout}");
        }
    }

    Ok(())
}

pub fn link_shared_library(lib_options: &LinkerOptions) -> Result<()> {
    let linker = find_linker()?;

    let mut args = Vec::new();

    args.push("--shared".to_string());

    args.push("-o".to_string());
    args.push(lib_options.output_path.clone());

    args.extend(lib_options.object_files.clone());

    for lib in &lib_options.libraries {
        args.push(format!("-l{lib}"));
    }

    if lib_options.verbose {
        args.push("-v".to_string());
    }

    if lib_options.verbose {
        eprintln!("Running linker for shared library: {linker} {args:?}");
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
    verbose: bool,
    link_libc: bool,
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
        verbose,
        link_libc,
    };

    if is_shared {
        link_shared_library(&options)
    } else {
        link_executable(&options)
    }
}
