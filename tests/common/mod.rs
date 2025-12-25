pub struct Test {
    name: String,
    files: Vec<(String, String)>,
    should_compile: Option<bool>,
    expected_ir: Option<String>,
    execute: Option<&'static dyn FnOnce(ExecutionResult)>,
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

    pub fn execute(&mut self, f: &'static (impl FnOnce(ExecutionResult) + 'static)) -> &mut Self {
        self.execute = Some(f);
        self
    }
}

pub struct ExecutionResult {
    exit_code: i32,
    stdout: String,
    stderr: String,
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
    fn drop(&mut self) {}
}

pub fn it(name: &str, f: impl FnOnce(&mut Test)) {
    let mut test = Test::new(name);
    f(&mut test);
}
