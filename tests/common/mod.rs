use oxic::{
    backend::{linker::linkers::LdLinker, BackendOptions},
    codegen::{self, CompileOptions, EmitType},
    errors::ErrorLevel,
    lexer::tokenize,
    parser::parse,
    ERRORS,
};
use std::{
    env, fs,
    hash::{DefaultHasher, Hash, Hasher},
    marker::PhantomData,
    path::{Path, PathBuf},
    process::Command,
};

pub struct Test {
    name: String,
    files: Vec<(String, String)>,
    should_compile: Option<bool>,
    expected_ir: Option<String>,
    execute: Option<Box<dyn FnOnce(ExecutionResult)>>,
    fail_on_level: ErrorLevel,
}

impl Test {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            files: vec![],
            should_compile: None,
            expected_ir: None,
            execute: None,
            fail_on_level: ErrorLevel::Warning,
        }
    }

    pub fn add_source(&mut self, source: &str) -> &mut Self {
        self.files
            .push(("main.oxi".to_string(), source.trim().to_string()));
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

    pub fn fail_on_level(&mut self, level: ErrorLevel) -> &mut Self {
        self.fail_on_level = level;
        self
    }
}

#[allow(dead_code)]
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

    #[allow(dead_code)]
    pub fn stderr(&self, expected: &str) -> &Self {
        assert_eq!(self.stderr, expected);
        self
    }
}

impl Drop for Test {
    fn drop(&mut self) {
        let temp_dir = PathBuf::from(".oxi/tests");
        let mut hasher = DefaultHasher::new();
        self.name.hash(&mut hasher);
        let hash = format!("{:016x}", hasher.finish());
        let test_dir = temp_dir.join(&hash);

        if env::var("OXI_DEBUG_TESTS").is_ok() {
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

        let (tokens, module_id) = match tokenize(source, &main_path) {
            Ok(t) => t,
            Err(e) => {
                if self.should_compile == Some(false) {
                    return;
                }
                panic!("Tokenization failed: {}", e);
            }
        };

        check_for_errors(self);

        let ast = match parse(tokens) {
            Ok(a) => a,
            Err(e) => {
                if self.should_compile == Some(false) {
                    return;
                }
                panic!("Parsing failed: {}", e);
            }
        };

        check_for_errors(self);

        let module_name = "main";
        let opts = CompileOptions {
            module_name,
            module_id,
            source_dir: &test_dir,
            output_file: &output_path,
            source_file: &main_path,
            emit: &EmitType::Llvm,
            backend_options: &BackendOptions::default(),
            pie: true,
            static_linking: false,
            linker_kind: PhantomData::<LdLinker>,
            cache_dir: Some(&test_dir),
        };

        match codegen::compile(ast.clone(), &opts) {
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

        check_for_errors(self);

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

                if env::var("OXI_DEBUG_TESTS").is_ok() {
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
                module_id,
                module_name: "main",
                source_dir: &test_dir,
                output_file: &exe_path,
                source_file: &main_path,
                emit: &EmitType::Executable,
                backend_options: &BackendOptions::default(),
                pie: true,
                static_linking: false,
                linker_kind: PhantomData::<LdLinker>,
                cache_dir: Some(&test_dir),
            };

            if let Err(e) = codegen::compile(ast, &exe_opts) {
                panic!("Failed to compile executable: {}", e);
            }

            check_for_errors(self);

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

        if env::var("OXI_DEBUG_TESTS").is_err() {
            let _ = fs::remove_dir_all(&test_dir);
        }
    }
}

fn check_for_errors(test: &Test) {
    if ERRORS.with(|e| e.borrow().has_errors_above_level(test.fail_on_level)) {
        ERRORS.with(|e| e.borrow().print_errors(ErrorLevel::Warning));
        if test.should_compile == Some(false) {
            return;
        }
        panic!("Compilation had errors or warnings above threshold");
    }
}

pub fn it(name: &str, f: impl FnOnce(&mut Test)) {
    let mut test = Test::new(name);
    f(&mut test);
}
