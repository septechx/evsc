use std::collections::HashMap;

use crate::{
    ast::{
        Ast, Expr, ExprKind, Stmt, StmtKind, Type, TypeKind,
        statements::{FnDeclStmt, StructDeclStmt, VarDeclStmt},
    },
    hir::{
        Def, DefId, ExportEntry, ExprId, Function, HirCrate, HirExpr, HirStmt, HirType, LocalId,
        MethodMeta, ModuleId, ModuleInfo, StmtId, Struct, TypeId, Variable,
        interner::{Interner, Symbol},
        resolve::{PendingImport, ResolutionStatus},
    },
};

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
                struct_methods: HashMap::new(),
            };
            self.krate.modules.push(modinfo);
        }

        self.collect_definitions(&asts);
        self.resolve_all_imports(&asts);
        self.lower_bodies(asts);
    }

    fn collect_definitions(&mut self, asts: &[Ast]) {
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

                        let mut method_map = HashMap::new();
                        for method in s.methods.iter() {
                            let method_sym = self.krate.interner.intern(&method.fn_decl.name.value);
                            let method_defid = self.alloc_def_placeholder();
                            method_map.insert(
                                method_sym,
                                MethodMeta {
                                    def: method_defid,
                                    is_static: method.is_static,
                                    public: method.is_public,
                                },
                            );
                        }
                        self.krate.modules[mid]
                            .struct_methods
                            .insert(defid, method_map);
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
    }

    fn resolve_all_imports(&mut self, asts: &[Ast]) {
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
    }

    fn lower_bodies(&mut self, asts: Vec<Ast>) {
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
            associated: None,
            static_method: false,
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
            methods: Vec::with_capacity(s.methods.len()),
            module: modid,
        };
        self.krate.defs[defid.0 as usize] = Def::Struct(st);

        if let Some(methods_map) = self.krate.modules[modid.0 as usize]
            .struct_methods
            .get(&defid)
            .cloned()
        {
            for method in s.methods.into_iter() {
                let method_sym = self.krate.interner.intern(&method.fn_decl.name.value);
                let meta = methods_map
                    .get(&method_sym)
                    .expect("method placeholder must exist");

                let method_defid = meta.def;

                let params: Vec<(Symbol, TypeId)> = method
                    .fn_decl
                    .arguments
                    .into_iter()
                    .map(|p| {
                        (
                            self.krate.interner.intern(&p.name.value),
                            self.lower_type(p.type_),
                        )
                    })
                    .collect();

                let ret = self.lower_type(method.fn_decl.return_type);

                let func = Function {
                    name: method_sym,
                    params,
                    ret,
                    body: None,
                    module: modid,
                    associated: Some(defid),
                    static_method: method.is_static,
                };

                self.krate.defs[method_defid.0 as usize] = Def::Function(func);

                if let Some(body) = method.fn_decl.body {
                    self.local_stack.push(HashMap::new());

                    let param_names: Vec<Symbol> = match &self.krate.defs[method_defid.0 as usize] {
                        Def::Function(func) => {
                            func.params.iter().map(|(pname, _)| *pname).collect()
                        }
                        _ => panic!("method is a function"),
                    };
                    for pname in param_names {
                        let local = self.alloc_local();
                        self.local_stack.last_mut().unwrap().insert(pname, local);
                    }

                    let expr = self.lower_expr(body);

                    if let Def::Function(func) = &mut self.krate.defs[method_defid.0 as usize] {
                        func.body = Some(expr);
                    }

                    self.local_stack.pop();
                }

                if let Def::Struct(st) = &mut self.krate.defs[defid.0 as usize] {
                    st.methods.push((method_sym, method_defid));
                } else {
                    panic!("struct defid must refer to Struct");
                }
            }
        }
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
            ExprKind::FunctionCall(fc) => match *fc.callee {
                Expr {
                    kind: ExprKind::MemberAccess(ma),
                    ..
                } => {
                    let base = self.lower_expr(*ma.base);
                    let method_sym = self.krate.interner.intern(&ma.member.value);
                    let args = fc
                        .arguments
                        .into_iter()
                        .map(|a| self.lower_expr(a))
                        .collect();
                    self.alloc_expr(HirExpr::MethodCall {
                        base,
                        method: method_sym,
                        args,
                    })
                }
                _ => {
                    let callee = self.lower_expr(*fc.callee);
                    let args = fc
                        .arguments
                        .into_iter()
                        .map(|a| self.lower_expr(a))
                        .collect();
                    self.alloc_expr(HirExpr::Call { callee, args })
                }
            },
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
            TypeKind::Pointer(p) => {
                let inner = *p.underlying;

                let (inner, mutable) = if let TypeKind::Mut(m) = inner.kind {
                    (*m.underlying, true)
                } else {
                    (inner, false)
                };

                let tid = self.lower_type(inner);

                self.alloc_type(HirType::Pointer(tid, mutable))
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
}
