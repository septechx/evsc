use crate::ast::{Ast, Expr, Stmt, Type, statements::BlockStmt};

pub trait Visitor {
    fn visit_stmt(&mut self, stmt: &mut Stmt);
    fn visit_expr(&mut self, expr: &mut Expr);
    fn visit_type(&mut self, ty: &mut Type);
}

pub trait Visitable {
    fn visit(&mut self, visitor: &mut impl Visitor);
}

impl Visitable for BlockStmt {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        visit_stmts(&mut self.body, visitor);
    }
}

impl Visitable for Ast {
    fn visit(&mut self, visitor: &mut impl Visitor) {
        visit_stmts(&mut self.0, visitor);
    }
}

fn visit_stmts(stmts: &mut [Stmt], _visitor: &mut impl Visitor) {
    for _stmt in stmts.iter_mut() {
        todo!("Call .visit + implement Visitable")
    }
}
