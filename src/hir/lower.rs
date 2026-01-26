use crate::{
    ast::{
        Ast, Expr, ExprKind, Stmt, StmtKind, Type, TypeKind, Visibility,
        statements::{FnDeclStmt, ImplStmt, InterfaceDeclStmt, StructDeclStmt, VarDeclStmt},
    },
    hashmap::FxHashMap,
    hir::{
        Def, DefId, ExportEntry, ExprId, Function, HirCrate, HirExpr, HirStmt, HirType, Interface,
        InterfaceMethod, LocalId, MethodMeta, ModuleId, ModuleInfo, StmtId, Struct, StructField,
        TypeId, Variable,
        interner::{Interner, Symbol},
        resolve::{PendingImport, ResolutionStatus},
    },
    lexer::token::TokenKind,
};

const BUILTIN_TYPES: [&str; 15] = [
    "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64", "f128",
    "bool", "void",
];

pub struct LoweringContext {
    pub krate: HirCrate,
    current_module: Option<ModuleId>,
    current_struct: Option<DefId>,
    local_stack: Vec<FxHashMap<Symbol, LocalId>>,
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
            current_struct: None,
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
                exports: FxHashMap::default(),
                items: Vec::new(),
                imports: FxHashMap::default(),
                struct_methods: FxHashMap::default(),
                struct_fields: FxHashMap::default(),
                struct_impls: FxHashMap::default(),
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
            let modid = ModuleId(mid as u32);
            for stmt in ast.items.iter() {
                match &stmt.kind {
                    StmtKind::FnDecl(f) => {
                        let sym = self.krate.interner.intern(&f.name.value);
                        let defid = self.alloc_def_placeholder(modid);
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                visibility: f.visibility,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);
                    }
                    StmtKind::StructDecl(s) => {
                        let sym = self.krate.interner.intern(&s.name.value);
                        let defid = self.alloc_def_placeholder(modid);
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                visibility: s.visibility,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);

                        let mut method_map = FxHashMap::default();
                        for method in s.methods.iter() {
                            let method_sym = self.krate.interner.intern(&method.fn_decl.name.value);
                            let method_defid = self.alloc_def_placeholder(modid);
                            method_map.insert(
                                method_sym,
                                MethodMeta {
                                    def: method_defid,
                                    is_static: method.is_static,
                                    visibility: method.visibility,
                                },
                            );
                        }
                        self.krate.modules[mid]
                            .struct_methods
                            .insert(defid, method_map);

                        let mut field_map = FxHashMap::default();
                        for field in s.fields.iter() {
                            let field_sym = self.krate.interner.intern(&field.name.value);
                            field_map.insert(field_sym, field.visibility);
                        }
                        self.krate.modules[mid]
                            .struct_fields
                            .insert(defid, field_map);
                    }
                    StmtKind::InterfaceDecl(i) => {
                        let sym = self.krate.interner.intern(&i.name.value);
                        let defid = self.alloc_def_placeholder(modid);
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                visibility: i.visibility,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);
                    }
                    StmtKind::Impl(_) => {} // Processed in lowering pass 3
                    StmtKind::VarDecl(v) => {
                        let sym = self.krate.interner.intern(&v.variable_name.value);
                        let defid = self.alloc_def_placeholder(modid);
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                visibility: v.visibility,
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
                        import_stmt: im,
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
                match self.try_resolve_import(pi.module_idx, pi.import_stmt) {
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
                    ResolutionStatus::Pending => {
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
                    StmtKind::InterfaceDecl(i) => self.lower_interface_decl(i),
                    StmtKind::Impl(im) => self.lower_impl_stmt(im),
                    StmtKind::VarDecl(v) => self.lower_top_var_decl(v),
                    StmtKind::Import(_) => {} // Processed in lowering pass 2
                    _ => todo!("Lowering of {:?} not implemented (PASS 3)", stmt.kind),
                }
            }
        }
    }

    fn alloc_def_placeholder(&mut self, modid: ModuleId) -> DefId {
        let defid = DefId(self.next_def);
        self.next_def += 1;
        self.krate.defs.push(Def::Placeholder(defid, modid));
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

    fn lower_fn_impl(
        &mut self,
        f: FnDeclStmt,
        defid: DefId,
        associated: Option<DefId>,
        is_static: bool,
    ) {
        let sym = self.krate.interner.intern(&f.name.value);
        let modid = self.current_module.expect("current module set");

        let params = f
            .arguments
            .into_iter()
            .map(|p| {
                (
                    self.krate.interner.intern(&p.name.value),
                    self.lower_type(p.ty),
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
            associated,
            static_method: is_static,
        };
        self.krate.defs[defid.0 as usize] = Def::Function(func);

        if let Some(body) = f.body {
            self.local_stack.push(FxHashMap::default());
            let param_names: Vec<_> = match &self.krate.defs[defid.0 as usize] {
                Def::Function(func) => func.params.iter().map(|(pname, _)| *pname).collect(),
                _ => panic!("expected function"),
            };
            for pname in param_names {
                let local = self.alloc_local();
                self.local_stack
                    .last_mut()
                    .expect("local stack exists")
                    .insert(pname, local);
            }
            let expr = self.lower_expr(body);
            if let Def::Function(func) = &mut self.krate.defs[defid.0 as usize] {
                func.body = Some(expr);
            }
            self.local_stack.pop();
        }
    }

    fn lower_fn_decl(&mut self, f: FnDeclStmt) {
        let sym = self.krate.interner.intern(&f.name.value);
        let defid = self.lookup_in_current_module(sym).expect("def must exist");
        self.lower_fn_impl(f, defid, None, false);
    }

    fn lower_struct_decl(&mut self, s: StructDeclStmt) {
        let sym = self.krate.interner.intern(&s.name.value);
        let modid = self.current_module.expect("current module set");
        let defid = self.lookup_in_current_module(sym).expect("def must exist");

        let fields = s
            .fields
            .into_iter()
            .map(|p| StructField {
                name: self.krate.interner.intern(&p.name.value),
                ty: self.lower_type(p.ty),
                visibility: p.visibility,
            })
            .collect();

        let st = Struct {
            name: sym,
            fields,
            methods: Vec::with_capacity(s.methods.len()),
            module: modid,
        };
        self.krate.defs[defid.0 as usize] = Def::Struct(st);

        let prev_struct = self.current_struct;
        self.current_struct = Some(defid);

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

                self.lower_fn_impl(method.fn_decl, method_defid, Some(defid), method.is_static);

                if let Def::Struct(st) = &mut self.krate.defs[defid.0 as usize] {
                    st.methods.push((method_sym, method_defid));
                } else {
                    panic!("struct defid must refer to Struct");
                }
            }
        }

        self.current_struct = prev_struct;
    }

    fn lower_interface_decl(&mut self, i: InterfaceDeclStmt) {
        let sym = self.krate.interner.intern(&i.name.value);
        let modid = self.current_module.expect("current module set");
        let defid = self
            .lookup_in_current_module(sym)
            .expect("interface def must exist");

        let mut methods = Vec::with_capacity(i.methods.len());
        for m in i.methods.into_iter() {
            let method_name = self.krate.interner.intern(&m.fn_decl.name.value);

            let param_tys = m
                .fn_decl
                .arguments
                .into_iter()
                .map(|arg| self.lower_type(arg.ty))
                .collect::<Vec<_>>();

            let ret_ty = self.lower_type(m.fn_decl.return_type);

            methods.push(InterfaceMethod {
                name: method_name,
                params: param_tys,
                ret: ret_ty,
            })
        }

        let iface = Interface {
            name: sym,
            module: modid,
            methods,
        };
        self.krate.defs[defid.0 as usize] = Def::Interface(iface);
    }

    fn lower_impl_stmt(&mut self, im: ImplStmt) {
        let interface_sym = self.krate.interner.intern(&im.interface.value);
        let interface_def = match self.lookup_in_current_module(interface_sym) {
            Some(def) => def,
            None => {
                self.krate
                    .diagnostics
                    .push(format!("Unknown interface `{}`", im.interface.value));
                return;
            }
        };

        let self_defid = match im.self_ty.kind {
            TypeKind::Symbol(s) => {
                let sym = self.krate.interner.intern(&s.name.value);
                match self.lookup_in_current_module(sym) {
                    Some(def) => def,
                    None => {
                        self.krate
                            .diagnostics
                            .push(format!("Unknown type `{}` in impl", s.name.value));
                        return;
                    }
                }
            }
            _ => {
                self.krate
                    .diagnostics
                    .push("Impl target must be a named type".to_string());
                return;
            }
        };

        let struct_modid = self.krate.defs[self_defid.0 as usize].module();
        match &self.krate.defs[self_defid.0 as usize] {
            Def::Struct(_) => {}
            _ => {
                self.krate
                    .diagnostics
                    .push("Impl target is not a struct".to_string());
                return;
            }
        }

        for method in im.items.into_iter() {
            let method_sym = self.krate.interner.intern(&method.fn_decl.name.value);
            let method_defid =
                self.alloc_def_placeholder(self.current_module.expect("current module set"));

            self.lower_fn_impl(
                method.fn_decl.clone(),
                method_defid,
                Some(self_defid),
                false,
            );

            let method_map = &mut self.krate.modules[struct_modid.0 as usize]
                .struct_methods
                .entry(self_defid)
                .or_default();

            method_map.insert(
                method_sym,
                MethodMeta {
                    def: method_defid,
                    is_static: false,
                    visibility: Visibility::Public,
                },
            );
        }

        self.krate.modules[struct_modid.0 as usize]
            .struct_impls
            .entry(self_defid)
            .or_default()
            .push(interface_def);
    }

    fn lower_top_var_decl(&mut self, v: VarDeclStmt) {
        let sym = self.krate.interner.intern(&v.variable_name.value);
        let modid = self.current_module.expect("current module set");
        let defid = self.lookup_in_current_module(sym).expect("def must exist");

        let ty = if let TypeKind::Infer = v.ty.kind {
            None
        } else {
            Some(self.lower_type(v.ty))
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
                } if ma.operator.kind == TokenKind::Dot => {
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
                    let mut fields = Vec::with_capacity(si.fields.len());
                    for (ident, val) in si.fields.into_iter() {
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
                if ma.operator.kind == TokenKind::ColonColon {
                    let base_id = self.lower_expr(*ma.base);
                    let member_sym = self.krate.interner.intern(&ma.member.value);

                    if let HirExpr::Global(defid) = &self.krate.exprs[base_id.0 as usize] {
                        let defid = *defid;
                        let struct_mod = self.krate.defs[defid.0 as usize].module();

                        if let Some(methods) = self.krate.modules[struct_mod.0 as usize]
                            .struct_methods
                            .get(&defid)
                            && let Some(meta) = methods.get(&member_sym)
                        {
                            if meta.visibility == Visibility::Private {
                                let allowed = self.current_struct == Some(defid);

                                if !allowed {
                                    self.krate.diagnostics.push(format!(
                                        "Cannot access private associated item `{}`",
                                        ma.member.value
                                    ));
                                }
                            }
                            return self.alloc_expr(HirExpr::Global(meta.def));
                        }

                        // Also could be a module, but we don't have good module-as-value yet in HIR.
                        // Assuming simple associated item access for now.
                    }

                    self.krate.diagnostics.push(format!(
                        "Cannot resolve associated item `{}`",
                        ma.member.value
                    ));
                    self.alloc_expr(HirExpr::Error)
                } else {
                    let base = self.lower_expr(*ma.base);
                    let field_sym = self.krate.interner.intern(&ma.member.value);
                    self.alloc_expr(HirExpr::Field {
                        base,
                        field: field_sym,
                    })
                }
            }
            ExprKind::Block(b) => {
                self.local_stack.push(FxHashMap::default());
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
                let ty = if let TypeKind::Infer = v.ty.kind {
                    None
                } else {
                    Some(self.lower_type(v.ty))
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

                if BUILTIN_TYPES.contains(&name.as_ref()) {
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
                let tid = self.lower_type(inner);
                self.alloc_type(HirType::Pointer(tid, p.mutability))
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
