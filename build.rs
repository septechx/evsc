use std::process::Command;

fn main() {
    let llvm_cxxflags = Command::new("llvm-config")
        .arg("--cxxflags")
        .output()
        .expect("Failed to run llvm-config --cxxflags")
        .stdout;

    let llvm_cxxflags =
        String::from_utf8(llvm_cxxflags).expect("Failed to parse llvm-config output");

    let mut build = cc::Build::new();

    for flag in llvm_cxxflags.split_whitespace() {
        build.flag(flag);
    }

    build
        .cpp(true)
        .file("include/llvm_bindings.cpp")
        .pic(true)
        .compile("llvm_bindings");

    println!("cargo:rerun-if-changed=include/llvm_bindings.cpp");
    println!("cargo:rerun-if-changed=build.rs");
}
