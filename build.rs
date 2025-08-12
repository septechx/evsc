use std::env;

fn main() {
    let include_path = env::current_dir().unwrap().join("include");
    println!("cargo:rustc-link-search=native={}", include_path.display());
}
