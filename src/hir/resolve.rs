use crate::{
    ast::{ImportTreeKind, statements::ImportStmt},
    hir::{ExportEntry, lower::LoweringContext},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolutionStatus {
    /// Successfully resolved and applied to the module
    Resolved,
    /// Failed permanently
    Failed,
    /// Temporary failure (might succeed in later pass)
    RetryLater,
}

pub struct PendingImport {
    pub module_idx: usize,
    pub import_stmt: ImportStmt,
}

impl LoweringContext {
    pub fn try_resolve_import(&mut self, mid: usize, im: &ImportStmt) -> ResolutionStatus {
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
