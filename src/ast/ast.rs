use std::any::Any;

pub trait Stmt: std::fmt::Debug {}

pub trait Expr: std::fmt::Debug {
    fn clone_box(&self) -> Box<dyn Expr>;
    fn as_any(&self) -> &dyn Any;
}

pub trait Type: std::fmt::Debug {
    fn clone_box(&self) -> Box<dyn Type>;
}
