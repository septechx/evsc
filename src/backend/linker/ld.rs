use std::process::Command;

use crate::{errors::ErrorLevel, ERRORS};

use super::Linker;

#[derive(Debug, Clone)]
pub struct LdLinker {
    args: Vec<String>,
    command: String,
}

impl LdLinker {
    fn find_linker() -> String {
        let linkers = ["ld.mold", "ld.lld", "ld.gold", "ld"];

        for linker in &linkers {
            if Command::new(linker).arg("--version").output().is_ok() {
                return linker.to_string();
            }
        }

        ERRORS.lock().add_simple(
            ErrorLevel::Fatal,
            format!("No suitable linker found. Tried: {}", linkers.join(", ")),
        );

        unreachable!()
    }
}

impl Linker for LdLinker {
    fn new() -> Self {
        let command = Self::find_linker();
        Self {
            args: vec![
                "--dynamic-linker".to_string(),
                "/usr/lib/ld-linux-x86-64.so.2".to_string(),
            ],
            command,
        }
    }

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
        self.args.push("--static".to_string());
    }

    fn add_shared(&mut self) {
        self.args.push("--shared".to_string());
    }

    fn add_pie(&mut self) {
        self.args.push("--pie".to_string());
    }

    fn command(&self) -> &str {
        &self.command
    }

    fn args(&self) -> &[String] {
        &self.args
    }
}
