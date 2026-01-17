use std::path::PathBuf;

/// Returns the filesystem root path used by the program.
///
/// If the `OXI_ROOT` environment variable is set, its value is returned as a `PathBuf`.
/// Otherwise, the root is derived from the current executable path by removing its
/// filename and its parent directory (moving up two components).
///
/// # Examples
///
/// ```
/// use std::path::PathBuf;
/// std::env::set_var("OXI_ROOT", "/tmp/myroot");
/// let root = crate::get_root();
/// assert_eq!(root, PathBuf::from("/tmp/myroot"));
/// std::env::remove_var("OXI_ROOT");
/// ```
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