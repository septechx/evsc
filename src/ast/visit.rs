use crate::ast::{
    Ast, Expr, Stmt, StmtKind, Type,
    statements::{BlockStmt, FnDeclStmt},
};

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

fn visit_stmts(stmts: &mut [Stmt], visitor: &mut impl Visitor) {
    for stmt in stmts.iter_mut() {
        match &mut stmt.kind {
            StmtKind::Block(block) => {
                block.visit(visitor);
            }
            StmtKind::Expression(expr) => {
                visitor.visit_expr(&mut expr.expression);
            }
            StmtKind::VarDecl(var_decl) => {
                if let Some(assigned_value) = &mut var_decl.assigned_value {
                    visitor.visit_expr(assigned_value);
                }
                if let Some(explicit_type) = &mut var_decl.explicit_type {
                    visitor.visit_type(explicit_type);
                }
            }
            StmtKind::StructDecl(struct_decl) => {
                for prop in &mut struct_decl.properties {
                    visitor.visit_type(&mut prop.explicit_type);
                }
                for method in &mut struct_decl.methods {
                    visit_fn_decl(&mut method.fn_decl, visitor);
                }
            }
            StmtKind::InterfaceDecl(interface_decl) => {
                for method in &mut interface_decl.methods {
                    visit_fn_decl(&mut method.fn_decl, visitor);
                }
            }
            StmtKind::FnDecl(fn_decl) => {
                visit_fn_decl(fn_decl, visitor);
            }
            StmtKind::Return(ret) => {
                if let Some(value) = &mut ret.value {
                    visitor.visit_expr(value);
                }
            }
        }
    }
}

fn visit_fn_decl(fn_decl: &mut FnDeclStmt, visitor: &mut impl Visitor) {
    for arg in &mut fn_decl.arguments {
        if let Some(explicit_type) = &mut arg.explicit_type {
            visitor.visit_type(explicit_type);
        }
    }
    visitor.visit_type(&mut fn_decl.return_type);
    visit_stmts(&mut fn_decl.body, visitor);
}
