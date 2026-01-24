use std::{
    env, fs,
    path::{Path, PathBuf},
};

use anyhow::Result;

use crate::{
    fatal_at_with_info,
    span::{PackageId, Span},
    utils::get_root,
};

pub fn resolve_std_lib(requester_span: Span, requeter_mod_id: PackageId) -> Result<PathBuf> {
    let env_var = env::var("OXI_LIB_PATH");
    if let Ok(env_var) = env_var {
        return Ok(Path::new(&env_var).join("std/lib.oxi"));
    }

    let root = get_root();
    let path = root.join("lib/oxi/std/lib.oxi");
    if fs::exists(&path).unwrap_or(false) {
        return Ok(path);
    }

    fatal_at_with_info!(
        requester_span,
        requeter_mod_id,
        "Could not find standard library",
        format!(
            "Set OXI_LIB_PATH to the stdlib root (containing std/lib.oxi), or place it in {}/lib/oxi/std/lib.oxi",
            root.display()
        )
    )
}
