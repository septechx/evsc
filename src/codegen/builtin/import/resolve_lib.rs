use std::{
    env, fs,
    path::{Path, PathBuf},
};

use anyhow::Result;

use crate::{
    errors::{
        builders,
        widgets::{CodeWidget, InfoWidget, LocationWidget},
    },
    span::{ModuleId, Span},
    utils::get_root,
};

pub fn resolve_std_lib(requester_span: Span, requeter_mod_id: ModuleId) -> Result<PathBuf> {
    let env_var = env::var("OXI_LIB_PATH");
    if let Ok(env_var) = env_var {
        return Ok(Path::new(&env_var).join("std/lib.oxi"));
    }

    let root = get_root();
    let path = root.join("lib/oxi/std/lib.oxi");
    if fs::exists(&path).unwrap_or(false) {
        return Ok(path);
    }

    crate::ERRORS.with(|e| -> Result<()> {
        e.borrow_mut().add(
            builders::fatal("Could not find standard library")
                .add_widget(LocationWidget::new(requester_span, requeter_mod_id)?)
                .add_widget(CodeWidget::new(requester_span, requeter_mod_id)?)
                .add_widget(InfoWidget::new(requester_span, requeter_mod_id,
                format!(
                    "Set OXI_LIB_PATH to the stdlib root (containing std/lib.oxi), or place it in {}/lib/oxi/std/lib.oxi",
                    root.display()
                ),
            )?),
        );
        Ok(())
    }).expect("failed to create error");
    unreachable!()
}
