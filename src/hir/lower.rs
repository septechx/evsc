use std::collections::HashMap;

use crate::{
    ast::{
        Ast, Expr, ExprKind, ImportTreeKind, Stmt, StmtKind, Type, TypeKind,
        statements::{FnDeclStmt, ImportStmt, StructDeclStmt, VarDeclStmt},
    },
    hir::{
        Def, DefId, ExportEntry, ExprId, Function, HirCrate, HirExpr, HirStmt, HirType, LocalId,
        ModuleId, ModuleInfo, StmtId, Struct, TypeId, Variable,
        interner::{Interner, Symbol},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResolutionStatus {
    /// Successfully resolved and applied to the module
    Resolved,
    /// Failed permanently
    Failed,
    /// Temporary failure (might succeed in later pass)
    RetryLater,
}

struct PendingImport {
    module_idx: usize,
    import_stmt: ImportStmt,
}

pub struct LoweringContext {
    pub krate: HirCrate,
    pub current_module: Option<ModuleId>,
    local_stack: Vec<HashMap<Symbol, LocalId>>,
    next_def: u32,
    next_expr: u32,
    next_type: u32,
    next_local: u32,
    next_stmt: u32,
}

impl LoweringContext {
    pub fn new() -> Self {
        LoweringContext {
            krate: HirCrate {
                modules: Vec::new(),
                defs: Vec::new(),
                exprs: Vec::new(),
                types: Vec::new(),
                stmts: Vec::new(),
                interner: Interner::new(),
                diagnostics: Vec::new(),
            },
            current_module: None,
            local_stack: Vec::new(),
            next_def: 0,
            next_expr: 0,
            next_type: 0,
            next_local: 0,
            next_stmt: 0,
        }
    }

    pub fn lower_crate(&mut self, asts: Vec<Ast>) {
        for ast in &asts {
            let modinfo = ModuleInfo {
                name: ast.name.to_string(),
                exports: HashMap::new(),
                items: Vec::new(),
                imports: HashMap::new(),
            };
            self.krate.modules.push(modinfo);
        }

        // PASS 1: Collect top-level definitions
        for (mid, ast) in asts.iter().enumerate() {
            for stmt in ast.items.iter() {
                match &stmt.kind {
                    StmtKind::FnDecl(f) => {
                        let sym = self.krate.interner.intern(&f.name.value);
                        let defid = self.alloc_def_placeholder();
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                public: f.is_public,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);
                    }
                    StmtKind::StructDecl(s) => {
                        let sym = self.krate.interner.intern(&s.name.value);
                        let defid = self.alloc_def_placeholder();
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                public: s.is_public,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);
                    }
                    StmtKind::VarDecl(v) => {
                        let sym = self.krate.interner.intern(&v.variable_name.value);
                        let defid = self.alloc_def_placeholder();
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                public: v.is_public,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);
                    }
                    StmtKind::Import(_) => {} // Processed in lowering pass 2
                    _ => todo!("Lowering of {:?} not implemented (PASS 1)", stmt.kind),
                }
            }
        }

        // PASS 2: Resolve imports (iteratively until fixpoint)
        let mut pending: Vec<PendingImport> = Vec::new();
        for (mid, ast) in asts.iter().enumerate() {
            for stmt in ast.items.iter() {
                if let StmtKind::Import(im) = &stmt.kind {
                    pending.push(PendingImport {
                        module_idx: mid,
                        import_stmt: im.clone(),
                    });
                }
            }
        }

        // attempt to resolve until no further progress
        let mut progress = true;
        while progress && !pending.is_empty() {
            progress = false;

            // iterate with index so we can remove resolved entries in-place
            let mut i = 0usize;
            while i < pending.len() {
                let pi = &pending[i];
                // try resolve; if resolved we remove from pending and set progress = true
                match self.try_resolve_import(pi.module_idx, &pi.import_stmt) {
                    ResolutionStatus::Resolved => {
                        pending.swap_remove(i);
                        progress = true;
                        continue;
                    }
                    ResolutionStatus::Failed => {
                        pending.swap_remove(i);
                        progress = true;
                        continue;
                    }
                    ResolutionStatus::RetryLater => {
                        // cannot resolve yet: keep item for next pass
                        i += 1;
                        continue;
                    }
                }
            }
        }

        // anything left unresolved -> emit diagnostics
        if !pending.is_empty() {
            for pi in pending {
                let segments: Vec<String> = pi
                    .import_stmt
                    .tree
                    .prefix
                    .segments
                    .iter()
                    .map(|ident| ident.value.to_string())
                    .collect();
                let path = segments.join("::");
                self.krate.diagnostics.push(format!(
                    "Could not resolve import `{}` in module `{}`",
                    path, self.krate.modules[pi.module_idx].name
                ));
            }
        }

        // PASS 3: Lower definition bodies
        for (mid, ast) in asts.iter().enumerate() {
            self.current_module = Some(ModuleId(mid as u32));
            for stmt in ast.items.iter().cloned() {
                match stmt.kind {
                    StmtKind::FnDecl(f) => self.lower_fn_decl(f),
                    StmtKind::StructDecl(s) => self.lower_struct_decl(s),
                    StmtKind::VarDecl(v) => self.lower_top_var_decl(v),
                    StmtKind::Import(_) => {} // Processed in lowering pass 2
                    _ => todo!("Lowering of {:?} not implemented (PASS 3)", stmt.kind),
                }
            }
        }
    }

    fn alloc_def_placeholder(&mut self) -> DefId {
        let defid = DefId(self.next_def);
        self.next_def += 1;
        self.krate.defs.push(Def::Placeholder(defid));
        defid
    }

    fn alloc_expr(&mut self, expr: HirExpr) -> ExprId {
        let id = ExprId(self.next_expr);
        self.next_expr += 1;
        self.krate.exprs.push(expr);
        id
    }

    fn alloc_type(&mut self, ty: HirType) -> TypeId {
        let id = TypeId(self.next_type);
        self.next_type += 1;
        self.krate.types.push(ty);
        id
    }

    fn alloc_local(&mut self) -> LocalId {
        let id = LocalId(self.next_local);
        self.next_local += 1;
        id
    }

    fn alloc_stmt(&mut self, stmt: HirStmt) -> StmtId {
        let id = StmtId(self.next_stmt);
        self.next_stmt += 1;
        self.krate.stmts.push(stmt);
        id
    }

    fn lower_fn_decl(&mut self, f: FnDeclStmt) {
        let sym = self.krate.interner.intern(&f.name.value);
        let modid = self.current_module.expect("current module set");
        let defid = self.lookup_in_current_module(sym).expect("def must exist");

        let params = f
            .arguments
            .into_iter()
            .map(|p| {
                (
                    self.krate.interner.intern(&p.name.value),
                    self.lower_type(p.type_),
                )
            })
            .collect();

        let ret = self.lower_type(f.return_type);

        let func = Function {
            name: sym,
            params,
            ret,
            body: None,
            module: modid,
        };
        self.krate.defs[defid.0 as usize] = Def::Function(func);

        if let Some(body) = f.body {
            self.local_stack.push(HashMap::new());
            let param_names: Vec<_> = match &self.krate.defs[defid.0 as usize] {
                Def::Function(func) => func.params.iter().map(|(pname, _)| *pname).collect(),
                _ => panic!("expected function"),
            };
            for pname in param_names {
                let local = self.alloc_local();
                self.local_stack.last_mut().unwrap().insert(pname, local);
            }
            let expr = self.lower_expr(body);
            if let Def::Function(func) = &mut self.krate.defs[defid.0 as usize] {
                func.body = Some(expr);
            }
            self.local_stack.pop();
        }
    }

    fn lower_struct_decl(&mut self, s: StructDeclStmt) {
        let sym = self.krate.interner.intern(&s.name.value);
        let modid = self.current_module.expect("current module set");
        let defid = self.lookup_in_current_module(sym).expect("def must exist");

        let fields = s
            .properties
            .into_iter()
            .map(|p| {
                (
                    self.krate.interner.intern(&p.name.value),
                    self.lower_type(p.type_),
                )
            })
            .collect();

        let st = Struct {
            name: sym,
            fields,
            module: modid,
        };
        self.krate.defs[defid.0 as usize] = Def::Struct(st);
    }

    fn lower_top_var_decl(&mut self, v: VarDeclStmt) {
        let sym = self.krate.interner.intern(&v.variable_name.value);
        let modid = self.current_module.expect("current module set");
        let defid = self.lookup_in_current_module(sym).expect("def must exist");

        let ty = if let TypeKind::Infer = v.type_.kind {
            None
        } else {
            Some(self.lower_type(v.type_))
        };
        let init = v.assigned_value.map(|e| self.lower_expr(e));
        let var = Variable {
            name: sym,
            ty,
            init,
            module: modid,
        };
        self.krate.defs[defid.0 as usize] = Def::Variable(var);
    }

    fn lower_expr(&mut self, expr: Expr) -> ExprId {
        match expr.kind {
            ExprKind::Literal(l) => self.alloc_expr(HirExpr::Literal(l)),
            ExprKind::Symbol(s) => {
                let sym = self.krate.interner.intern(&s.value.value);

                for scope in self.local_stack.iter().rev() {
                    if let Some(local) = scope.get(&sym) {
                        return self.alloc_expr(HirExpr::Local(*local));
                    }
                }

                if let Some(defid) = self.lookup_in_current_module(sym) {
                    return self.alloc_expr(HirExpr::Global(defid));
                }

                self.krate
                    .diagnostics
                    .push(format!("Unknown symbol {}", s.value.value));
                self.alloc_expr(HirExpr::Error)
            }
            ExprKind::FunctionCall(fc) => {
                let callee = self.lower_expr(*fc.callee);
                let args = fc
                    .arguments
                    .into_iter()
                    .map(|a| self.lower_expr(a))
                    .collect();
                self.alloc_expr(HirExpr::Call { callee, args })
            }
            ExprKind::StructInstantiation(si) => {
                let def_sym = self.krate.interner.intern(&si.name.value);
                if let Some(defid) = self.lookup_in_current_module(def_sym) {
                    let mut fields = Vec::with_capacity(si.properties.len());
                    for (ident, val) in si.properties.into_iter() {
                        let fsym = self.krate.interner.intern(&ident.value);
                        let v = self.lower_expr(val);
                        fields.push((fsym, v));
                    }
                    self.alloc_expr(HirExpr::StructInit { def: defid, fields })
                } else {
                    self.krate
                        .diagnostics
                        .push(format!("Unknown struct name: {}", si.name.value));
                    self.alloc_expr(HirExpr::Error)
                }
            }
            ExprKind::MemberAccess(ma) => {
                let base = self.lower_expr(*ma.base);
                let field_sym = self.krate.interner.intern(&ma.member.value);
                self.alloc_expr(HirExpr::Field {
                    base,
                    field: field_sym,
                })
            }
            ExprKind::Block(b) => {
                self.local_stack.push(HashMap::new());
                let stmt_ids = b
                    .body
                    .into_iter()
                    .map(|stmt| self.lower_stmt(stmt))
                    .collect();
                self.local_stack.pop();
                self.alloc_expr(HirExpr::Block { stmts: stmt_ids })
            }
            ExprKind::Binary(b) => {
                let left_id = self.lower_expr(*b.left);
                let right_id = self.lower_expr(*b.right);
                let op = b.operator.kind.into();
                self.alloc_expr(HirExpr::Binary {
                    left: left_id,
                    op,
                    right: right_id,
                })
            }
            _ => todo!("Lowering of {:?} not implemented", expr.kind),
        }
    }

    fn lower_stmt(&mut self, stmt: Stmt) -> StmtId {
        match stmt.kind {
            StmtKind::Expression(e) => {
                let exprid = self.lower_expr(e.expression);
                self.alloc_stmt(HirStmt::Expr(exprid))
            }
            StmtKind::VarDecl(v) => {
                let name = self.krate.interner.intern(&v.variable_name.value);
                let ty = if let TypeKind::Infer = v.type_.kind {
                    None
                } else {
                    Some(self.lower_type(v.type_))
                };
                let init_expr = v.assigned_value.map(|e| self.lower_expr(e));

                let local = self.alloc_local();

                if let Some(scope) = self.local_stack.last_mut() {
                    scope.insert(name, local);
                }

                let var = HirStmt::Let {
                    name,
                    ty,
                    init: init_expr.unwrap_or_else(|| self.alloc_expr(HirExpr::Error)),
                    local,
                };
                self.alloc_stmt(var)
            }
            StmtKind::Return(r) => {
                let val = r.value.map(|e| self.lower_expr(e));
                self.alloc_stmt(HirStmt::Return(val))
            }
            _ => todo!("Lowering of {:?} not implemented", stmt.kind),
        }
    }

    fn lower_type(&mut self, ty: Type) -> TypeId {
        match ty.kind {
            TypeKind::Symbol(s) => {
                let name = s.name.value;

                let builtin = [
                    "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32",
                    "f64", "f128", "bool", "void",
                ];
                if builtin.contains(&&*name) {
                    return self.alloc_type(HirType::Builtin(name.into()));
                }

                let sym = self.krate.interner.intern(&name);
                if let Some(defid) = self.lookup_in_current_module(sym) {
                    return self.alloc_type(HirType::Adt(defid));
                }

                self.krate
                    .diagnostics
                    .push(format!("Unknown type: {}", name));
                self.alloc_type(HirType::Error)
            }
            _ => todo!("Lowering of {:?} not implemented", ty.kind),
        }
    }

    fn lookup_in_current_module(&self, sym: Symbol) -> Option<DefId> {
        let modid = self.current_module?;
        let module = &self.krate.modules[modid.0 as usize];

        // Check local items before imports
        if let Some(export_entry) = module.exports.get(&sym) {
            return Some(export_entry.def);
        }

        if let Some(defid) = module.imports.get(&sym) {
            return Some(*defid);
        }

        None
    }

    fn try_resolve_import(&mut self, mid: usize, im: &ImportStmt) -> ResolutionStatus {
        match &im.tree.kind {
            ImportTreeKind::Simple(rename_opt) => {
                let segments = &im.tree.prefix.segments;
                if segments.is_empty() {
                    self.krate.diagnostics.push(format!(
                        "Empty import in module {}",
                        self.krate.modules[mid].name
                    ));
                    return ResolutionStatus::Failed;
                }

                let desired_local_name = match rename_opt {
                    Some(ident) => ident.value.as_ref(),
                    None => segments.last().unwrap().value.as_ref(),
                };
                let local_sym = self.krate.interner.intern(desired_local_name);

                if self.krate.modules[mid].imports.contains_key(&local_sym)
                    || self.krate.modules[mid].exports.contains_key(&local_sym)
                {
                    self.krate.diagnostics.push(format!(
                        "Import name collision: `{}` in module `{}`",
                        desired_local_name, self.krate.modules[mid].name
                    ));
                    return ResolutionStatus::Failed;
                }

                if segments.len() == 1 {
                    let name = &segments[0].value;
                    let sym = self.krate.interner.intern(name);
                    let mut found: Option<(usize, ExportEntry)> = None;
                    for (i, m) in self.krate.modules.iter().enumerate() {
                        if let Some(entry) = m.exports.get(&sym) {
                            found = Some((i, entry.clone()));
                            break;
                        }
                    }

                    if let Some((def_mod_idx, export_entry)) = found {
                        if def_mod_idx != mid && !export_entry.public {
                            self.krate.diagnostics.push(format!(
                                "Cannot import `{}` as it is not marked as public in module `{}`",
                                name, self.krate.modules[def_mod_idx].name
                            ));
                            return ResolutionStatus::Failed;
                        }

                        self.krate.modules[mid]
                            .imports
                            .insert(local_sym, export_entry.def);

                        if im.is_public {
                            if def_mod_idx != mid && !export_entry.public {
                                self.krate.diagnostics.push(format!(
                                    "Cannot re-export `{}` from `{}` because original is private",
                                    name, self.krate.modules[def_mod_idx].name
                                ));
                            } else {
                                self.krate.modules[mid].exports.insert(
                                    local_sym,
                                    ExportEntry {
                                        def: export_entry.def,
                                        public: true,
                                    },
                                );
                            }
                        }

                        ResolutionStatus::Resolved
                    } else {
                        ResolutionStatus::RetryLater
                    }
                } else {
                    let module_name = &segments[0].value;
                    let symbol_name = &segments[segments.len() - 1].value;

                    // find the module index if it exists
                    let target_idx_opt = self
                        .krate
                        .modules
                        .iter()
                        .position(|m| m.name == module_name.to_string());
                    if let Some(tmid) = target_idx_opt {
                        let sym = self.krate.interner.intern(symbol_name);
                        let maybe_export = self.krate.modules[tmid]
                            .exports
                            .get(&sym)
                            .map(|export| (export.def, export.public));
                        if let Some((def, public)) = maybe_export {
                            if tmid != mid && !public {
                                self.krate.diagnostics.push(format!(
                                    "Cannot import `{}` from module `{}` as it is not marked as public",
                                    symbol_name, module_name
                                ));
                                return ResolutionStatus::Failed;
                            }

                            let local_name = match rename_opt {
                                Some(ident) => self.krate.interner.intern(&ident.value),
                                None => self.krate.interner.intern(symbol_name),
                            };
                            self.krate.modules[mid].imports.insert(local_name, def);

                            if im.is_public {
                                self.krate.modules[mid]
                                    .exports
                                    .insert(local_sym, ExportEntry { def, public: true });
                            }

                            ResolutionStatus::Resolved
                        } else {
                            ResolutionStatus::RetryLater
                        }
                    } else {
                        self.krate
                            .diagnostics
                            .push(format!("Module {} not found for import", module_name));
                        ResolutionStatus::Failed
                    }
                }
            }
            _ => {
                self.krate
                    .diagnostics
                    .push("Unsupported import tree (only simple imports supported)".to_string());
                ResolutionStatus::Failed
            }
        }
    }
}
