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

/// Resolve the filesystem path to the standard library used by the compiler.
///
/// Attempts to locate the standard library in the following order:
/// 1. If the `OXI_LIB_PATH` environment variable is set, returns `<OXI_LIB_PATH>/std/lib.oxi`.
/// 2. Otherwise, returns `<root>/lib/oxi/std/lib.oxi` where `root` is obtained from `get_root()` if that file exists.
/// If neither location yields an existing file, records a fatal diagnostic referencing `requester_span` and `requeter_mod_id` and then panics while reporting the error.
///
/// # Parameters
///
/// - `requester_span`: Source code span used to position error widgets when reporting a missing standard library.
/// - `requeter_mod_id`: Module identifier used to position error widgets when reporting a missing standard library.
///
/// # Returns
///
/// The resolved `PathBuf` pointing to the standard library file (`lib.oxi`) on success.
///
/// # Panics
///
/// Panics after registering a fatal error if the standard library cannot be found in either the environment-provided path or the default project layout.
///
/// # Examples
///
/// ```
/// // Example assumes `span` and `mod_id` are available in the calling context.
/// // let path = resolve_std_lib(span, mod_id).unwrap();
/// // assert!(path.ends_with("lib.oxi"));
/// ```
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
                format!("Add OXI_LIB_PATH environment variable to point to the standard library, or place it in {}/lib/oxi/", root.display()),
            )?),
        );
        Ok(())
    }).expect("failed to create error");
    unreachable!()
}