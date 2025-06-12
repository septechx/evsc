pub trait Stmt: std::fmt::Debug {
    fn stmt(&self) -> ();
}

pub trait Expr: std::fmt::Debug {
    fn expr(&self) -> ();
    fn clone_box(&self) -> Box<dyn Expr>;
}
