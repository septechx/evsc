use std::{
    env, fs,
    path::{Path, PathBuf},
};

use anyhow::{bail, Result};

pub fn resolve_std_lib() -> Result<PathBuf> {
    let env_var = env::var("EVSC_LIB_PATH");
    if env_var.is_ok() {
        let env_var = env_var.unwrap();
        return Ok(Path::new(&env_var).join("std/std.evsc"));
    }

    if fs::exists("/usr/local/share/evsc/lib/std/std.evsc").is_ok() {
        return Ok(PathBuf::from("~/.local/share/evsc/lib/std/std.evsc"));
    }

    if fs::exists("/opt/evsc/lib/std/std.evsc").is_ok() {
        return Ok(PathBuf::from("opt/evsc/lib/std/std.evsc"));
    }

    bail!("Could not find std.evsc");
}
