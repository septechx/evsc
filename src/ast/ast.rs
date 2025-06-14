pub trait Stmt: std::fmt::Debug {}

pub trait Expr: std::fmt::Debug {
    fn clone_box(&self) -> Box<dyn Expr>;
}

pub trait Type: std::fmt::Debug {
    fn clone_box(&self) -> Box<dyn Type>;
}
