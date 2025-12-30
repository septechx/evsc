use std::{
    env, fs,
    path::{Path, PathBuf},
};

use anyhow::Result;

use crate::errors::{InfoBlock, builders};

pub fn resolve_std_lib() -> Result<PathBuf> {
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

    crate::ERRORS.with(|e| {
        e.borrow_mut().add(
            builders::fatal("Could not find standard library").with_info(InfoBlock::new(
                "Add EVSC_LIB_PATH environment variable to point to the standard library, or place it in /usr/share/evsc or /opt/evsc",
            )),
        );
    });
    unreachable!()
}
