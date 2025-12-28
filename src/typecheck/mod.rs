mod stage1;

mod checker;
pub use checker::TypeChecker;

use crate::ast::Statement;

pub trait Check {
    fn check(&self, ast: &[Statement]);
}

pub trait Stage {
    fn run_checks(&self, ast: &[Statement]);
}

#[macro_export]
macro_rules! stage {
    ($id:literal : $($checks:ident),*) => {paste::paste! {
        pub struct [<Stage $id>] {
            checks: Vec<Box<dyn Check>>,
        }

        impl [<Stage $id>] {
            pub fn new() -> Self {
                Self { checks: vec![$(Box::new($checks::default())),*] }
            }
        }

        impl Stage for [<Stage $id>] {
            fn run_checks(&self, ast: &[Statement]) {
                for check in &self.checks {
                    check.check(ast);
                }
            }
        }
    }};
}
