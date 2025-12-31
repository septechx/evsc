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
};

pub fn resolve_std_lib(requester_span: Span, requeter_mod_id: ModuleId) -> Result<PathBuf> {
    let env_var = env::var("EVSC_LIB_PATH");
    if let Ok(env_var) = env_var {
        return Ok(Path::new(&env_var).join("std/std.evsc"));
    }

    let possible_paths: [&str; _] = [
        "/usr/share/evsc/lib/std/std.evsc",
        "/opt/evsc/lib/std/std.evsc",
    ];
    for path in possible_paths.iter() {
        if fs::exists(path).unwrap_or(false) {
            return Ok(PathBuf::from(path));
        }
    }

    crate::ERRORS.with(|e| -> Result<()> {
        e.borrow_mut().add(
            builders::fatal("Could not find standard library")
                .add_widget(LocationWidget::new(requester_span, requeter_mod_id)?)
                .add_widget(CodeWidget::new(requester_span, requeter_mod_id)?)
                .add_widget(InfoWidget::new(requester_span, requeter_mod_id,
                "Add EVSC_LIB_PATH environment variable to point to the standard library, or place it in /usr/share/evsc or /opt/evsc",
            )?),
        );
        Ok(())
    }).expect("failed to create error");
    unreachable!()
}
