use crate::{
    ast::{
        AssocItem, AssocItemKind, Ast, Expr, ExprKind, Fn, Ident, Item, ItemKind, Stmt, StmtKind,
        visit::{VisitAction, Visitable, Visitor},
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
                                format!("First definition of `{}` here", ident.value),
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

    fn validate_fn_decl(&mut self, f: &Fn) {
        self.check_duplicate_names(f.parameters.iter().map(|a| &a.0), "function parameters");

        if f.is_extern {
            if f.body.is_some() {
                error_at!(
                    f.name.span,
                    self.module_id,
                    "Extern functions cannot have a body"
                )
                .expect("failed to emit error");
            }
        } else if f.body.is_none() {
            error_at!(
                f.name.span,
                self.module_id,
                "Non-extern function must have a body"
            )
            .expect("failed to emit error");
        }

        if let Some(body) = &f.body {
            let old_in_function = self.in_function;
            let old_top_level = self.is_top_level;
            self.in_function = true;
            self.is_top_level = false;
            body.visit(self);
            self.in_function = old_in_function;
            self.is_top_level = old_top_level;
        }
    }

    fn validate_assoc_item(&mut self, item: &AssocItem) {
        match &item.kind {
            AssocItemKind::Fn(f) => self.validate_fn_decl(f),
        }
    }
}

impl Visitor for AstValidator {
    fn visit_item(&mut self, item: &Item) -> VisitAction {
        match &item.kind {
            ItemKind::Fn(f) => {
                self.validate_fn_decl(f);
                VisitAction::SkipChildren
            }
            ItemKind::Impl { items, .. } => {
                let old_top_level = self.is_top_level;
                self.is_top_level = false;
                for item in items.iter() {
                    self.validate_assoc_item(item);
                }
                self.is_top_level = old_top_level;
                VisitAction::SkipChildren
            }
            ItemKind::Struct { fields, items, .. } => {
                self.check_duplicate_names(fields.iter().map(|f| &f.0), "struct fields");
                self.check_duplicate_names(
                    items.iter().map(|item| match &item.kind {
                        AssocItemKind::Fn(f) => &f.name,
                    }),
                    "struct methods",
                );

                let old_top_level = self.is_top_level;
                self.is_top_level = false;
                for item in items.iter() {
                    self.validate_assoc_item(item);
                }
                self.is_top_level = old_top_level;

                VisitAction::SkipChildren
            }
            ItemKind::Interface { items, .. } => {
                self.check_duplicate_names(
                    items.iter().map(|item| match &item.kind {
                        AssocItemKind::Fn(f) => &f.name,
                    }),
                    "interface methods",
                );

                let old_top_level = self.is_top_level;
                self.is_top_level = false;
                for item in items.iter() {
                    self.validate_assoc_item(item);
                }
                self.is_top_level = old_top_level;

                VisitAction::SkipChildren
            }
            ItemKind::Static { .. } => VisitAction::Continue,
            ItemKind::Import(_) => VisitAction::Continue,
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> VisitAction {
        match &stmt.kind {
            StmtKind::Return(ret) => {
                if !self.in_function {
                    error_at!(
                        stmt.span,
                        self.module_id,
                        "Return statement outside of function"
                    )
                    .expect("failed to emit error");
                }
                if let Some(val) = ret {
                    val.visit(self);
                }

                VisitAction::SkipChildren
            }
            StmtKind::Let {
                name: _,
                ty: _,
                value,
                mutability: _,
            } => {
                if let Some(val) = value {
                    val.visit(self);
                }

                VisitAction::SkipChildren
            }
            _ => VisitAction::Continue,
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> VisitAction {
        match &expr.kind {
            ExprKind::StructInstantiation { name: _, fields } => {
                let mut seen = FxHashMap::default();
                for (ident, val) in fields.iter() {
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
                                        format!("First initialization of `{}` here", ident.value),
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

                VisitAction::SkipChildren
            }
            ExprKind::Block(b) => {
                let old_top_level = self.is_top_level;
                self.is_top_level = false;
                for stmt in b.stmts.iter() {
                    stmt.visit(self);
                }
                self.is_top_level = old_top_level;

                VisitAction::SkipChildren
            }
            _ => VisitAction::Continue,
        }
    }
}

pub fn validate_ast(ast: &Ast, module_id: ModuleId) {
    let mut validator = AstValidator {
        module_id,
        in_function: false,
        is_top_level: true,
    };

    let mut top_level_names = Vec::new();
    for item in ast.items.iter() {
        match &item.kind {
            ItemKind::Fn(f) => top_level_names.push(&f.name),
            ItemKind::Struct { name, .. } => top_level_names.push(name),
            ItemKind::Interface { name, .. } => top_level_names.push(name),
            ItemKind::Static { name, .. } => top_level_names.push(name),
            ItemKind::Impl { .. } | ItemKind::Import(_) => {}
        }
    }
    validator.check_duplicate_names(top_level_names, "module scope");

    ast.visit(&mut validator);
}
