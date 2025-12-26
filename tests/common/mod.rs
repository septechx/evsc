use evscc::{
    ERRORS,
    backend::BackendOptions,
    errors::ErrorLevel,
    intermediate::{self, CompileOptions, EmitType},
    lexer::tokenize,
    parser::parse,
    typecheck::TypeChecker,
};
use std::{
    env, fs,
    hash::{DefaultHasher, Hash, Hasher},
    path::{Path, PathBuf},
    process::Command,
};

pub struct Test {
    name: String,
    files: Vec<(String, String)>,
    should_compile: Option<bool>,
    expected_ir: Option<String>,
    execute: Option<Box<dyn FnOnce(ExecutionResult)>>,
}

impl Test {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            files: vec![],
            should_compile: None,
            expected_ir: None,
            execute: None,
        }
    }

    pub fn add_source(&mut self, source: &str) -> &mut Self {
        self.files
            .push(("main.evsc".to_string(), source.trim().to_string()));
        self
    }

    pub fn compiles(&mut self, should_succeed: bool) -> &mut Self {
        self.should_compile = Some(should_succeed);
        self
    }

    pub fn ir_eq(&mut self, expected: &str) -> &mut Self {
        self.expected_ir = Some(expected.to_string());
        self
    }

    pub fn execute<F>(&mut self, f: F) -> &mut Self
    where
        F: FnOnce(ExecutionResult) + 'static,
    {
        self.execute = Some(Box::new(f));
        self
    }
}

pub struct ExecutionResult {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
}

impl ExecutionResult {
    pub fn exit_code(&self, expected: i32) -> &Self {
        assert_eq!(self.exit_code, expected);
        self
    }

    pub fn stdout(&self, expected: &str) -> &Self {
        assert_eq!(self.stdout, expected);
        self
    }

    pub fn stderr(&self, expected: &str) -> &Self {
        assert_eq!(self.stderr, expected);
        self
    }
}

impl Drop for Test {
    fn drop(&mut self) {
        let temp_dir = PathBuf::from(".evsc/tests");
        let mut hasher = DefaultHasher::new();
        self.name.hash(&mut hasher);
        let hash = format!("{:016x}", hasher.finish());
        let test_dir = temp_dir.join(&hash);

        if env::var("EVSC_DEBUG_TESTS").is_ok() {
            eprintln!(
                "Test files kept at: {} (test: '{}')",
                test_dir.display(),
                self.name
            );
        }

        if let Err(e) = fs::create_dir_all(&test_dir) {
            panic!("Failed to create test directory: {}", e);
        }

        for (filename, content) in &self.files {
            let file_path = test_dir.join(filename);
            if let Err(e) = fs::write(&file_path, content) {
                panic!("Failed to write source file {}: {}", filename, e);
            }
        }

        let main_file = self.files.first().unwrap();
        let main_path = test_dir.join(&main_file.0);
        let output_path = test_dir.join("output.ll");

        let source = match fs::read_to_string(&main_path) {
            Ok(s) => s,
            Err(e) => panic!("Failed to read source file: {}", e),
        };

        let tokens = match tokenize(source, &main_path) {
            Ok(t) => t,
            Err(e) => {
                if self.should_compile == Some(false) {
                    return;
                }
                panic!("Tokenization failed: {}", e);
            }
        };

        check_for_errors(self.should_compile);

        let ast = match parse(tokens.clone()) {
            Ok(a) => a,
            Err(e) => {
                if self.should_compile == Some(false) {
                    return;
                }
                panic!("Parsing failed: {}", e);
            }
        };

        check_for_errors(self.should_compile);

        let typechecker = TypeChecker::new(main_path.clone(), tokens);
        match typechecker.check(&ast.body) {
            Ok(_) => {}
            Err(e) => {
                if self.should_compile == Some(false) {
                    return;
                }
                panic!("Type checking failed: {}", e);
            }
        }

        check_for_errors(self.should_compile);

        let module_name = "main";
        let opts = CompileOptions {
            module_name,
            source_dir: &test_dir,
            output_file: &output_path,
            source_file: &main_path,
            emit: &EmitType::Llvm,
            backend_options: &BackendOptions::default(),
            pic: true,
            linker_kind: None,
            cache_dir: Some(&test_dir),
        };

        match intermediate::compile(ast.clone(), &opts) {
            Ok(_) => {
                if self.should_compile == Some(false) {
                    panic!("Compilation succeeded but was expected to fail");
                }
            }
            Err(e) => {
                if self.should_compile == Some(false) {
                    return;
                }
                panic!("Compilation failed: {}", e);
            }
        }

        check_for_errors(self.should_compile);

        if let Some(expected_ir_file) = &self.expected_ir {
            let expected_path = Path::new("tests").join(expected_ir_file);
            let expected_content = match fs::read_to_string(&expected_path) {
                Ok(c) => c,
                Err(e) => panic!(
                    "Failed to read expected IR file {}: {}",
                    expected_ir_file, e
                ),
            };

            let actual_content = match fs::read_to_string(&output_path) {
                Ok(c) => c,
                Err(e) => panic!("Failed to read generated IR: {}", e),
            };

            if expected_content != actual_content {
                println!("IR mismatch for test '{}'", self.name);
                println!("Expected: {}", expected_path.display());
                println!("Actual: {}", output_path.display());

                let diff = similar::TextDiff::from_lines(&expected_content, &actual_content);
                println!("\nDiff:");
                for change in diff.iter_all_changes() {
                    let sign = match change.tag() {
                        similar::ChangeTag::Delete => "-",
                        similar::ChangeTag::Insert => "+",
                        similar::ChangeTag::Equal => " ",
                    };
                    print!("{}{}", sign, change);
                }

                if env::var("EVSC_DEBUG_TESTS").is_ok() {
                    let mut hasher = DefaultHasher::new();
                    self.name.hash(&mut hasher);
                    let hash = format!("{:016x}", hasher.finish());
                    let test_dir_path = temp_dir.join(&hash);
                    println!(
                        "\nTest files kept at: {} (test: '{}')",
                        test_dir_path.display(),
                        self.name
                    );
                    return;
                }

                panic!("IR output does not match expected");
            }
        }

        if let Some(execute_fn) = self.execute.take() {
            let exe_path = test_dir.join("main");
            let exe_opts = CompileOptions {
                module_name: "main",
                source_dir: &test_dir,
                output_file: &exe_path,
                source_file: &main_path,
                emit: &EmitType::Executable,
                backend_options: &BackendOptions::default(),
                pic: true,
                linker_kind: Some(evscc::backend::LinkerKind::Ld),
                cache_dir: Some(&test_dir),
            };

            if let Err(e) = intermediate::compile(ast, &exe_opts) {
                panic!("Failed to compile executable: {}", e);
            }

            check_for_errors(self.should_compile);

            let output = match Command::new(&exe_path).output() {
                Ok(o) => o,
                Err(e) => {
                    eprintln!("Tried to execute: {exe_path:?}");
                    panic!("Failed to execute test: {}", e);
                }
            };

            let result = ExecutionResult {
                exit_code: output.status.code().unwrap_or(-1),
                stdout: String::from_utf8_lossy(&output.stdout).to_string(),
                stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            };

            execute_fn(result);
        }

        if env::var("EVSC_DEBUG_TESTS").is_err() {
            let _ = fs::remove_dir_all(&test_dir);
        }
    }
}

fn check_for_errors(should_compile: Option<bool>) {
    if ERRORS.with(|e| e.collector.borrow().has_errors()) {
        ERRORS.with(|e| e.collector.borrow().print_errors(ErrorLevel::Error));
        if should_compile == Some(false) {
            return;
        }
        panic!("Compilation had errors");
    }
}

pub fn it(name: &str, f: impl FnOnce(&mut Test)) {
    let mut test = Test::new(name);
    f(&mut test);
}
