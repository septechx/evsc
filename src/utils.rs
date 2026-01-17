use std::path::PathBuf;

pub fn get_root() -> PathBuf {
    if let Some(override_root) = std::env::var_os("OXI_ROOT") {
        return PathBuf::from(override_root);
    }

    let mut exe = std::env::current_exe().expect("Failed to get current exe path");
    // /usr/bin/oxic → /usr/bin
    exe.pop();
    // /usr/bin → /usr
    exe.pop();
    exe
}
