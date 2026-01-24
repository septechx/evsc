use std::collections::HashMap;

use crate::{
    ast::{
        Ast, Expr, ExprKind, ImportTreeKind, Stmt, StmtKind, Type, TypeKind,
        statements::{FnDeclStmt, StructDeclStmt, VarDeclStmt},
    },
    hir::{
        Def, DefId, ExprId, Function, HirCrate, HirExpr, HirStmt, HirType, ImportEntry, LocalId,
        ModuleId, ModuleInfo, StmtId, Struct, TypeId, Variable,
        interner::{Interner, Symbol},
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
                imports: Vec::new(),
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
                        // TODO: Check for visibility
                        self.krate.modules[mid].exports.insert(sym, defid);
                        self.krate.modules[mid].items.push(defid);
                    }
                    StmtKind::StructDecl(s) => {
                        let sym = self.krate.interner.intern(&s.name.value);
                        let defid = self.alloc_def_placeholder();
                        // TODO: Check for visibility
                        self.krate.modules[mid].exports.insert(sym, defid);
                        self.krate.modules[mid].items.push(defid);
                    }
                    StmtKind::VarDecl(v) => {
                        let sym = self.krate.interner.intern(&v.variable_name.value);
                        let defid = self.alloc_def_placeholder();
                        // TODO: Check for visibility
                        self.krate.modules[mid].exports.insert(sym, defid);
                        self.krate.modules[mid].items.push(defid);
                    }
                    StmtKind::Import(_) => {} // Processed in lowering pass 2
                    _ => todo!("Lowering of {:?} not implemented (PASS 1)", stmt.kind),
                }
            }
        }

        // PASS 2: Resolve imports
        for (mid, ast) in asts.iter().enumerate() {
            for stmt in ast.items.iter() {
                if let StmtKind::Import(im) = &stmt.kind {
                    match &im.tree.kind {
                        ImportTreeKind::Simple(rename_opt) => {
                            let segments = &im.tree.prefix.segments;
                            if segments.is_empty() {
                                self.krate.diagnostics.push(format!(
                                    "Empty import in module {}",
                                    self.krate.modules[mid].name
                                ));
                                continue;
                            }

                            if segments.len() == 1 {
                                // Import a top-level `name`
                                let name = &segments[0].value;
                                let mut found_def: Option<DefId> = None;
                                for modinfo in &self.krate.modules {
                                    let sym = self.krate.interner.intern(name);
                                    if let Some(defid) = modinfo.exports.get(&sym) {
                                        found_def = Some(*defid);
                                        break;
                                    }
                                }
                                if let Some(defid) = found_def {
                                    let local_name = match rename_opt {
                                        Some(ident) => self.krate.interner.intern(&ident.value),
                                        None => self.krate.interner.intern(name),
                                    };
                                    self.krate.modules[mid].imports.push(ImportEntry {
                                        name: local_name,
                                        def: defid,
                                    });
                                    self.krate.modules[mid].exports.insert(local_name, defid);
                                } else {
                                    self.krate
                                        .diagnostics
                                        .push(format!("Import target not found: {}", name));
                                }
                            } else {
                                // Import a `module::Name`
                                let module_name = &segments[0].value;
                                let symbol_name = &segments[segments.len() - 1].value;
                                let mut target_mod: Option<usize> = None;
                                for (i, m) in self.krate.modules.iter().enumerate() {
                                    if m.name == module_name.to_string() {
                                        target_mod = Some(i);
                                        break;
                                    }
                                }
                                if let Some(tmid) = target_mod {
                                    let sym = self.krate.interner.intern(symbol_name);
                                    if let Some(&defid) = self.krate.modules[tmid].exports.get(&sym)
                                    {
                                        let local_name = match rename_opt {
                                            Some(ident) => self.krate.interner.intern(&ident.value),
                                            None => self.krate.interner.intern(symbol_name),
                                        };
                                        self.krate.modules[mid].imports.push(ImportEntry {
                                            name: local_name,
                                            def: defid,
                                        });
                                        // TODO: Check for visibility
                                        self.krate.modules[mid].exports.insert(local_name, defid);
                                    } else {
                                        self.krate.diagnostics.push(format!(
                                            "Symbol {} not found in module {}",
                                            symbol_name, module_name
                                        ));
                                    }
                                } else {
                                    self.krate.diagnostics.push(format!(
                                        "Module {} not found for import",
                                        module_name
                                    ));
                                }
                            }
                        }
                        _ => todo!("Lowering of {:?} not implemented (PASS 2)", im.tree.kind),
                    }
                }
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
        let defid = *self.krate.modules[modid.0 as usize]
            .exports
            .get(&sym)
            .expect("def must exist");

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
        let defid = *self.krate.modules[modid.0 as usize]
            .exports
            .get(&sym)
            .expect("def must exist");

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
        let defid = *self.krate.modules[modid.0 as usize]
            .exports
            .get(&sym)
            .expect("def must exist");

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

                let modid = self.current_module.expect("current module set");
                if let Some(defid) = self.krate.modules[modid.0 as usize].exports.get(&sym) {
                    return self.alloc_expr(HirExpr::Global(*defid));
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
                let modid = self.current_module.expect("current module set");
                if let Some(&defid) = self.krate.modules[modid.0 as usize].exports.get(&def_sym) {
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
                if let Some(modid) = self.current_module
                    && let Some(defid) = self.krate.modules[modid.0 as usize].exports.get(&sym)
                {
                    return self.alloc_type(HirType::Adt(*defid));
                }

                self.krate
                    .diagnostics
                    .push(format!("Unknown type: {}", name));
                self.alloc_type(HirType::Error)
            }
            _ => todo!("Lowering of {:?} not implemented", ty.kind),
        }
    }
}
