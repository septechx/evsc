use crate::{
    ast::{
        Ast, Expr, ExprKind, Ident, Stmt, StmtKind, Type, Visibility,
        statements::FnDeclStmt,
        visit::{Visitable, Visitor},
    },
    error_at,
    errors::{
        builders,
        widgets::{CodeWidget, HighlightType, InfoWidget, LocationWidget},
    },
    hashmap::FxHashMap,
    span::ModuleId,
};

struct AstValidator {
    module_id: ModuleId,
    in_function: bool,
    is_top_level: bool,
}

impl AstValidator {
    fn check_duplicate_names<'a, I>(&self, names: I, context: &str)
    where
        I: IntoIterator<Item = &'a Ident>,
    {
        let mut seen = FxHashMap::default();
        for ident in names {
            if let Some(first_span) = seen.insert(&ident.value, ident.span) {
                let msg = format!("Duplicate definition of `{}` in {}", ident.value, context);
                crate::ERRORS
                    .with(|e| -> anyhow::Result<()> {
                        let err = builders::error(msg)
                            .add_widget(LocationWidget::new(ident.span, self.module_id)?)
                            .add_widget(CodeWidget::new(
                                ident.span,
                                self.module_id,
                                HighlightType::Error,
                            )?)
                            .add_widget(InfoWidget::new(
                                first_span,
                                self.module_id,
                                format!("First definition of `{}` is here", ident.value),
                            )?)
                            .add_widget(LocationWidget::new(first_span, self.module_id)?)
                            .add_widget(CodeWidget::new(
                                first_span,
                                self.module_id,
                                HighlightType::Info,
                            )?);

                        e.borrow_mut().add(err);
                        Ok(())
                    })
                    .expect("failed to emit error");
            }
        }
    }

    fn validate_fn_decl(&mut self, f: &FnDeclStmt) {
        self.check_duplicate_names(f.arguments.iter().map(|a| &a.name), "function arguments");

        if f.is_extern {
            if f.body.is_some() {
                error_at!(
                    f.name.span,
                    self.module_id,
                    "Extern functions cannot have a body"
                )
                .expect("failed to emit error");
            }
        } else if self.is_top_level && f.body.is_none() {
            error_at!(
                f.name.span,
                self.module_id,
                "Non-extern function must have a body"
            )
            .expect("failed to emit error");
        }

        if let Some(body) = &f.body {
            if !matches!(body.kind, ExprKind::Block(_)) {
                error_at!(
                    body.span,
                    self.module_id,
                    "Function body must be a block expression"
                )
                .expect("failed to emit error");
            }

            let old_in_function = self.in_function;
            let old_top_level = self.is_top_level;
            self.in_function = true;
            self.is_top_level = false;
            body.visit(self);
            self.in_function = old_in_function;
            self.is_top_level = old_top_level;
        }
    }
}

impl Visitor for AstValidator {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::FnDecl(f) => {
                self.validate_fn_decl(f);
            }
            StmtKind::StructDecl(s) => {
                self.check_duplicate_names(s.fields.iter().map(|f| &f.name), "struct fields");
                self.check_duplicate_names(
                    s.methods.iter().map(|m| &m.fn_decl.name),
                    "struct methods",
                );

                let old_top_level = self.is_top_level;
                self.is_top_level = false;
                for method in s.methods.iter() {
                    self.validate_fn_decl(&method.fn_decl);
                }
                self.is_top_level = old_top_level;
            }
            StmtKind::InterfaceDecl(i) => {
                self.check_duplicate_names(
                    i.methods.iter().map(|m| &m.fn_decl.name),
                    "interface methods",
                );

                let old_top_level = self.is_top_level;
                self.is_top_level = false;
                for method in i.methods.iter() {
                    self.validate_fn_decl(&method.fn_decl);
                }
                self.is_top_level = old_top_level;
            }
            StmtKind::Return(r) => {
                if !self.in_function {
                    error_at!(
                        stmt.span,
                        self.module_id,
                        "Return statement outside of function"
                    )
                    .expect("failed to emit error");
                }
                if let Some(val) = &r.value {
                    val.visit(self);
                }
            }
            StmtKind::VarDecl(v) => {
                if !self.is_top_level && v.visibility == Visibility::Public {
                    error_at!(
                        stmt.span,
                        self.module_id,
                        "Visibility modifier `pub` is not allowed on local variables"
                    )
                    .expect("failed to emit error");
                }
                if let Some(val) = &v.assigned_value {
                    val.visit(self);
                }
            }
            StmtKind::Expression(e) => {
                e.expression.visit(self);
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::StructInstantiation(s) => {
                let mut seen = FxHashMap::default();
                for (ident, val) in s.fields.iter() {
                    if let Some(first_span) = seen.insert(&ident.value, ident.span) {
                        let msg =
                            format!("Duplicate field `{}` in struct instantiation", ident.value);
                        crate::ERRORS
                            .with(|e| -> anyhow::Result<()> {
                                let err = builders::error(msg)
                                    .add_widget(LocationWidget::new(ident.span, self.module_id)?)
                                    .add_widget(CodeWidget::new(
                                        ident.span,
                                        self.module_id,
                                        HighlightType::Error,
                                    )?)
                                    .add_widget(InfoWidget::new(
                                        first_span,
                                        self.module_id,
                                        format!(
                                            "First initialization of `{}` is here",
                                            ident.value
                                        ),
                                    )?)
                                    .add_widget(LocationWidget::new(first_span, self.module_id)?)
                                    .add_widget(CodeWidget::new(
                                        first_span,
                                        self.module_id,
                                        HighlightType::Info,
                                    )?);
                                e.borrow_mut().add(err);
                                Ok(())
                            })
                            .expect("failed to emit error");
                    }
                    val.visit(self);
                }
            }
            ExprKind::Block(b) => {
                let old_top_level = self.is_top_level;
                self.is_top_level = false;
                for stmt in b.body.iter() {
                    stmt.visit(self);
                }
                self.is_top_level = old_top_level;
            }
            _ => {}
        }
    }

    fn visit_type(&mut self, _ty: &Type) {}
}

pub fn validate_ast(ast: &Ast, module_id: ModuleId) {
    let mut validator = AstValidator {
        module_id,
        in_function: false,
        is_top_level: true,
    };

    let mut top_level_names = Vec::new();
    for stmt in ast.items.iter() {
        match &stmt.kind {
            StmtKind::FnDecl(f) => top_level_names.push(&f.name),
            StmtKind::StructDecl(s) => top_level_names.push(&s.name),
            StmtKind::InterfaceDecl(i) => top_level_names.push(&i.name),
            StmtKind::VarDecl(v) => {
                if v.is_static {
                    top_level_names.push(&v.variable_name);
                }
            }
            _ => {}
        }
    }
    validator.check_duplicate_names(top_level_names, "module scope");

    ast.visit(&mut validator);
}

