use std::process::Command;

fn main() {
    let output = Command::new("llvm-config")
        .arg("--cxxflags")
        .output()
        .expect("Failed to run llvm-config --cxxflags");

    if !output.status.success() {
        panic!(
            "llvm-config --cxxflags failed with error: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let llvm_cxxflags =
        String::from_utf8(output.stdout).expect("Failed to parse llvm-config output");

    let mut build = cc::Build::new();

    for flag in llvm_cxxflags.split_whitespace() {
        build.flag(flag);
    }

    build
        .cpp(true)
        .file("include/llvm_bindings.cpp")
        .pic(true)
        .compile("llvm_bindings");

    println!("cargo:rerun-if-changed=include/");
    println!("cargo:rerun-if-changed=include/llvm_bindings.cpp");
    println!("cargo:rerun-if-changed=build.rs");
}
