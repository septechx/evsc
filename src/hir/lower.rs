use thin_vec::{ThinVec, thin_vec};

use crate::{
    ast::{
        AssocItem, AssocItemKind, Ast, Expr, ExprKind, Fn, Ident, ItemKind, Stmt, StmtKind, Type,
        TypeKind, Visibility,
    },
    hashmap::FxHashMap,
    hir::{
        interner::Symbol,
        resolve::{PendingImport, ResolutionStatus},
        *,
    },
    lexer::token::TokenKind,
};

const BUILTIN_TYPES: [&str; 15] = [
    "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64", "f128",
    "bool", "void",
];

#[derive(Debug, Default)]
pub struct LoweringContext {
    pub krate: HirCrate,
    current_module: Option<ModuleId>,
    current_struct: Option<DefId>,
    local_stack: ThinVec<FxHashMap<Symbol, LocalId>>,
    next_def: u32,
    next_expr: u32,
    next_type: u32,
    next_local: u32,
    next_stmt: u32,
    next_body: u32,
}

impl LoweringContext {
    pub fn new() -> Self {
        LoweringContext::default()
    }

    pub fn lower_crate(&mut self, asts: ThinVec<Ast>) {
        for ast in &asts {
            let modinfo = ModuleInfo {
                name: ast.name.to_string(),
                exports: FxHashMap::default(),
                items: ThinVec::new(),
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
            for item in ast.items.iter() {
                match &item.kind {
                    ItemKind::Fn(f) => {
                        let sym = self.krate.interner.intern(&f.name.value);
                        let defid = self.alloc_def_placeholder(modid);
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                visibility: item.visibility,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);
                    }
                    ItemKind::Struct {
                        name,
                        fields,
                        items,
                    } => {
                        let sym = self.krate.interner.intern(&name.value);
                        let defid = self.alloc_def_placeholder(modid);
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                visibility: item.visibility,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);

                        let mut method_map = FxHashMap::default();
                        for item in items.iter() {
                            let AssocItemKind::Fn(fn_decl) = &item.kind;
                            let method_sym = self.krate.interner.intern(&fn_decl.name.value);
                            let method_defid = self.alloc_def_placeholder(modid);
                            method_map.insert(
                                method_sym,
                                MethodMeta {
                                    def: method_defid,
                                    is_static: item.is_static,
                                    visibility: item.visibility,
                                },
                            );
                        }
                        self.krate.modules[mid]
                            .struct_methods
                            .insert(defid, method_map);

                        let mut field_map = FxHashMap::default();
                        for field in fields.iter() {
                            let field_sym = self.krate.interner.intern(&field.0.value);
                            field_map.insert(field_sym, field.2);
                        }
                        self.krate.modules[mid]
                            .struct_fields
                            .insert(defid, field_map);
                    }
                    ItemKind::Interface { name, .. } => {
                        let sym = self.krate.interner.intern(&name.value);
                        let defid = self.alloc_def_placeholder(modid);
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                visibility: item.visibility,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);
                    }
                    ItemKind::Static { name, .. } => {
                        let sym = self.krate.interner.intern(&name.value);
                        let defid = self.alloc_def_placeholder(modid);
                        self.krate.modules[mid].exports.insert(
                            sym,
                            ExportEntry {
                                def: defid,
                                visibility: item.visibility,
                            },
                        );
                        self.krate.modules[mid].items.push(defid);
                    }
                    ItemKind::Impl { .. } => {} // Processed in lowering pass 3
                    ItemKind::Import(_) => {}   // Processed in lowering pass 2
                }
            }
        }
    }

    fn resolve_all_imports(&mut self, asts: &[Ast]) {
        // PASS 2: Resolve imports (iteratively until fixpoint)
        let mut pending: ThinVec<PendingImport> = ThinVec::new();
        for (mid, ast) in asts.iter().enumerate() {
            for item in ast.items.iter() {
                if let ItemKind::Import(im) = &item.kind {
                    pending.push(PendingImport {
                        module_idx: mid,
                        import_item: im,
                        visibility: item.visibility,
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
                match self.try_resolve_import(pi.module_idx, pi.import_item, pi.visibility) {
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
                let segments: ThinVec<String> = pi
                    .import_item
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

    fn lower_bodies(&mut self, asts: ThinVec<Ast>) {
        // PASS 3: Lower definition bodies
        for (mid, ast) in asts.iter().enumerate() {
            self.current_module = Some(ModuleId(mid as u32));
            for item in ast.items.iter().cloned() {
                match item.kind {
                    ItemKind::Fn(f) => self.lower_fn_decl(f),
                    ItemKind::Struct {
                        name,
                        fields,
                        items,
                    } => self.lower_struct_decl(name, fields, items),
                    ItemKind::Interface { name, items } => self.lower_interface_decl(name, items),
                    ItemKind::Impl {
                        self_ty,
                        interface,
                        items,
                    } => self.lower_impl_stmt(self_ty, interface, items),
                    ItemKind::Static { name, ty, value } => self.lower_static_item(name, ty, value),
                    ItemKind::Import(_) => {} // Processed in lowering pass 2
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

    fn alloc_body(&mut self, body: Body) -> BodyId {
        let id = BodyId(self.next_body);
        self.next_body += 1;
        self.krate.bodies.push(body);
        id
    }

    fn lower_fn_impl(&mut self, f: Fn, defid: DefId, associated: Option<DefId>, is_static: bool) {
        let sym = self.krate.interner.intern(&f.name.value);
        let modid = self.current_module.expect("current module set");

        let params = f
            .parameters
            .into_iter()
            .map(|(pname, pty)| {
                (
                    self.krate.interner.intern(&pname.value),
                    self.lower_type(pty),
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
            let param_names: ThinVec<_> = match &self.krate.defs[defid.0 as usize] {
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

            let stmt_ids: ThinVec<StmtId> = body
                .stmts
                .into_iter()
                .map(|stmt| self.lower_stmt(stmt))
                .collect();

            let body_id = self.alloc_body(Body { stmts: stmt_ids });
            if let Def::Function(func) = &mut self.krate.defs[defid.0 as usize] {
                func.body = Some(body_id);
            }
            self.local_stack.pop();
        }
    }

    fn lower_fn_decl(&mut self, f: Fn) {
        let sym = self.krate.interner.intern(&f.name.value);
        let defid = self.lookup_in_current_module(sym).expect("def must exist");
        self.lower_fn_impl(f, defid, None, false);
    }

    fn lower_struct_decl(
        &mut self,
        sname: Ident,
        sfields: ThinVec<(Ident, Type, Visibility)>,
        sitems: ThinVec<AssocItem>,
    ) {
        let sym = self.krate.interner.intern(&sname.value);
        let modid = self.current_module.expect("current module set");
        let defid = self.lookup_in_current_module(sym).expect("def must exist");

        let fields = sfields
            .into_iter()
            .map(|(fname, fty, fvis)| StructField {
                name: self.krate.interner.intern(&fname.value),
                ty: self.lower_type(fty),
                visibility: fvis,
            })
            .collect();

        let st = Struct {
            name: sym,
            fields,
            methods: ThinVec::with_capacity(sitems.len()),
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
            for item in sitems.into_iter() {
                let AssocItemKind::Fn(fn_decl) = item.kind;
                let method_sym = self.krate.interner.intern(&fn_decl.name.value);
                let meta = methods_map
                    .get(&method_sym)
                    .expect("method placeholder must exist");

                let method_defid = meta.def;

                self.lower_fn_impl(fn_decl, method_defid, Some(defid), item.is_static);

                if let Def::Struct(st) = &mut self.krate.defs[defid.0 as usize] {
                    st.methods.push((method_sym, method_defid));
                } else {
                    panic!("struct defid must refer to Struct");
                }
            }
        }

        self.current_struct = prev_struct;
    }

    fn lower_interface_decl(&mut self, iname: Ident, iitems: ThinVec<AssocItem>) {
        let sym = self.krate.interner.intern(&iname.value);
        let modid = self.current_module.expect("current module set");
        let defid = self
            .lookup_in_current_module(sym)
            .expect("interface def must exist");

        let mut methods = ThinVec::with_capacity(iitems.len());
        for item in iitems.into_iter() {
            let AssocItemKind::Fn(fn_decl) = item.kind;
            let method_name = self.krate.interner.intern(&fn_decl.name.value);

            let param_tys = fn_decl
                .parameters
                .into_iter()
                .map(|arg| self.lower_type(arg.1))
                .collect::<ThinVec<_>>();

            let ret_ty = self.lower_type(fn_decl.return_type);

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

    fn lower_impl_stmt(&mut self, self_ty: Type, iface: Ident, items: ThinVec<AssocItem>) {
        let interface_sym = self.krate.interner.intern(&iface.value);
        let interface_def = match self.lookup_in_current_module(interface_sym) {
            Some(def) => def,
            None => {
                self.krate
                    .diagnostics
                    .push(format!("Unknown interface `{}`", iface.value));
                return;
            }
        };

        let self_defid = match self_ty.kind {
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

        for item in items.into_iter() {
            let AssocItemKind::Fn(fn_decl) = item.kind;
            let method_sym = self.krate.interner.intern(&fn_decl.name.value);
            let method_defid =
                self.alloc_def_placeholder(self.current_module.expect("current module set"));

            self.lower_fn_impl(fn_decl, method_defid, Some(self_defid), item.is_static);

            let method_map = &mut self.krate.modules[struct_modid.0 as usize]
                .struct_methods
                .entry(self_defid)
                .or_default();

            method_map.insert(
                method_sym,
                MethodMeta {
                    def: method_defid,
                    is_static: item.is_static,
                    visibility: item.visibility,
                },
            );
        }

        self.krate.modules[struct_modid.0 as usize]
            .struct_impls
            .entry(self_defid)
            .or_default()
            .push(interface_def);
    }

    fn lower_static_item(&mut self, sname: Ident, sty: Type, svalue: Expr) {
        let sym = self.krate.interner.intern(&sname.value);
        let modid = self.current_module.expect("current module set");
        let defid = self.lookup_in_current_module(sym).expect("def must exist");

        let ty = if let TypeKind::Infer = sty.kind {
            None
        } else {
            Some(self.lower_type(sty))
        };
        let init = Some(self.lower_expr(svalue));
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
                let sym = self.krate.interner.intern(&s.value);

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
                    .push(format!("Unknown symbol {}", s.value));
                self.alloc_expr(HirExpr::Error)
            }
            ExprKind::FunctionCall { callee, parameters } => match *callee {
                Expr {
                    kind:
                        ExprKind::MemberAccess {
                            base,
                            member,
                            operator,
                        },
                    ..
                } if operator.kind == TokenKind::Dot => {
                    let base_id = self.lower_expr(*base);
                    let method_sym = self.krate.interner.intern(&member.value);
                    let args = parameters.into_iter().map(|a| self.lower_expr(a)).collect();
                    self.alloc_expr(HirExpr::MethodCall {
                        base: base_id,
                        method: method_sym,
                        args,
                    })
                }
                _ => {
                    let callee_id = self.lower_expr(*callee);
                    let args = parameters.into_iter().map(|a| self.lower_expr(a)).collect();
                    self.alloc_expr(HirExpr::Call {
                        callee: callee_id,
                        args,
                    })
                }
            },
            ExprKind::StructInstantiation {
                name,
                fields: expr_fields,
            } => {
                let def_sym = self.krate.interner.intern(&name.value);
                if let Some(defid) = self.lookup_in_current_module(def_sym) {
                    let mut fields = ThinVec::with_capacity(expr_fields.len());
                    for (ident, val) in expr_fields.into_iter() {
                        let fsym = self.krate.interner.intern(&ident.value);
                        let v = self.lower_expr(val);
                        fields.push((fsym, v));
                    }
                    self.alloc_expr(HirExpr::StructInit { def: defid, fields })
                } else {
                    self.krate
                        .diagnostics
                        .push(format!("Unknown struct name: {}", name.value));
                    self.alloc_expr(HirExpr::Error)
                }
            }
            ExprKind::MemberAccess {
                base,
                member,
                operator,
            } => {
                if operator.kind == TokenKind::ColonColon {
                    let base_id = self.lower_expr(*base);
                    let member_sym = self.krate.interner.intern(&member.value);

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
                                        member.value
                                    ));
                                }
                            }
                            return self.alloc_expr(HirExpr::Global(meta.def));
                        }

                        // Also could be a module, but we don't have good module-as-value yet in HIR.
                        // Assuming simple associated item access for now.
                    }

                    self.krate
                        .diagnostics
                        .push(format!("Cannot resolve associated item `{}`", member.value));
                    self.alloc_expr(HirExpr::Error)
                } else {
                    let base_id = self.lower_expr(*base);
                    let field_sym = self.krate.interner.intern(&member.value);
                    self.alloc_expr(HirExpr::Field {
                        base: base_id,
                        field: field_sym,
                    })
                }
            }
            ExprKind::Binary {
                left,
                operator,
                right,
            } => {
                let left_id = self.lower_expr(*left);
                let right_id = self.lower_expr(*right);
                let op = operator.kind.into();
                self.alloc_expr(HirExpr::Binary {
                    left: left_id,
                    op,
                    right: right_id,
                })
            }
            ExprKind::Block(b) => {
                let stmts = self.lower_body(b.stmts);
                self.alloc_expr(HirExpr::Block { stmts })
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.lower_expr(*condition);

                let then_stmts = self.lower_body(then_branch.stmts);
                let then_block = self.alloc_expr(HirExpr::Block { stmts: then_stmts });

                let else_block = else_branch.map(|e| self.lower_expr(*e));

                self.alloc_expr(HirExpr::If {
                    cond,
                    then_branch: then_block,
                    else_branch: else_block,
                })
            }
            ExprKind::While { condition, body } => {
                let cond = self.lower_expr(*condition);

                let then_stmts = self.lower_body(body.stmts);
                let then_block = self.alloc_expr(HirExpr::Block { stmts: then_stmts });

                let break_expr = self.alloc_expr(HirExpr::Break { value: None });
                let break_stmt = self.alloc_stmt(HirStmt::Semi(break_expr));
                let else_block = self.alloc_expr(HirExpr::Block {
                    stmts: thin_vec![break_stmt],
                });

                let if_expr = self.alloc_expr(HirExpr::If {
                    cond,
                    then_branch: then_block,
                    else_branch: Some(else_block),
                });
                let if_stmt = self.alloc_stmt(HirStmt::Semi(if_expr));
                let loop_body = self.alloc_body(Body {
                    stmts: thin_vec![if_stmt],
                });

                self.alloc_expr(HirExpr::Loop {
                    body: loop_body,
                    source: LoopSource::While,
                })
            }
            ExprKind::Loop(l) => {
                let stmts = self.lower_body(l.stmts);
                let body = self.alloc_body(Body { stmts });
                self.alloc_expr(HirExpr::Loop {
                    body,
                    source: LoopSource::Loop,
                })
            }
            ExprKind::Break(b) => {
                let value = b.map(|e| self.lower_expr(*e));
                self.alloc_expr(HirExpr::Break { value })
            }
            _ => todo!("Lowering of {:?} not implemented", expr.kind),
        }
    }

    fn lower_stmt(&mut self, stmt: Stmt) -> StmtId {
        match stmt.kind {
            StmtKind::Expr(expr) => {
                let exprid = self.lower_expr(expr);
                self.alloc_stmt(HirStmt::Expr(exprid))
            }
            StmtKind::Semi(expr) => {
                let exprid = self.lower_expr(expr);
                self.alloc_stmt(HirStmt::Semi(exprid))
            }
            StmtKind::Let {
                name,
                ty,
                value,
                mutability: _,
            } => {
                let name_sym = self.krate.interner.intern(&name.value);
                let ty_id = if let TypeKind::Infer = ty.kind {
                    None
                } else {
                    Some(self.lower_type(ty))
                };
                let init_expr = value.map(|e| self.lower_expr(e));

                let local = self.alloc_local();

                if let Some(scope) = self.local_stack.last_mut() {
                    scope.insert(name_sym, local);
                }

                let var = HirStmt::Let {
                    name: name_sym,
                    ty: ty_id,
                    init: init_expr.unwrap_or_else(|| self.alloc_expr(HirExpr::Error)),
                    local,
                };
                self.alloc_stmt(var)
            }
            StmtKind::Return(value) => {
                let val = value.map(|e| self.lower_expr(e));
                self.alloc_stmt(HirStmt::Return(val))
            }
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

    fn lower_body(&mut self, body: ThinVec<Stmt>) -> ThinVec<StmtId> {
        self.local_stack.push(FxHashMap::default());
        let stmts = body
            .into_iter()
            .map(|s| self.lower_stmt(s))
            .collect::<ThinVec<_>>();
        self.local_stack.pop();
        stmts
    }
}
