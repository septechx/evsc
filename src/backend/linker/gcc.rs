use anyhow::{anyhow, Result};
use std::process::Command;

use super::Linker;

#[derive(Debug, Clone)]
pub struct GccLinker {
    args: Vec<String>,
    command: String,
}

impl GccLinker {
    pub fn new() -> Result<Self> {
        let command = Self::find_linker()?;
        Ok(Self {
            args: Vec::new(),
            command,
        })
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
