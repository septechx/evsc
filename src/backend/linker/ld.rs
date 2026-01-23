use std::process::Command;

use crate::fatal;

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

        fatal!(format!(
            "No suitable linker found. Tried: {}",
            linkers.join(", ")
        ))
    }
}

impl Linker for LdLinker {
    fn new() -> Self {
        let command = Self::find_linker();
        Self {
            args: Vec::new(),
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

    fn add_no_pie(&mut self) {
        self.args.push("--no-pie".to_string());
    }

    fn add_dynamic_linker(&mut self) {
        self.args.push("--dynamic-linker".to_string());
        self.args.push("/lib64/ld-linux-x86-64.so.2".to_string());
    }

    fn command(&self) -> &str {
        &self.command
    }

    fn args(&self) -> &[String] {
        &self.args
    }
}
