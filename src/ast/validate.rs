use crate::{
    ast::{
        Ast, Expr, ExprKind, Stmt, StmtKind, Type,
        statements::FnDeclStmt,
        visit::{Visitable, Visitor},
    },
    error_at,
    span::ModuleId,
};

struct AstValidator {
    module_id: ModuleId,
}

impl AstValidator {
    fn validate_fn_decl(&self, f: &FnDeclStmt) {
        if let Some(body) = &f.body
            && !matches!(body.kind, ExprKind::Block(_))
        {
            error_at!(
                body.span,
                self.module_id,
                "Function body must be a block expression"
            )
            .expect("failed to create error");
        }
    }
}

impl Visitor for AstValidator {
    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::FnDecl(f) => {
                self.validate_fn_decl(f);
            }
            StmtKind::StructDecl(s) => {
                for method in s.methods.iter() {
                    self.validate_fn_decl(&method.fn_decl);
                }
            }
            StmtKind::InterfaceDecl(i) => {
                for method in i.methods.iter() {
                    self.validate_fn_decl(&method.fn_decl);
                }
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, _expr: &mut Expr) {}
    fn visit_type(&mut self, _ty: &mut Type) {}
}

pub fn validate_ast(ast: &mut Ast, module_id: ModuleId) {
    let mut validator = AstValidator { module_id };
    ast.visit(&mut validator);
}

