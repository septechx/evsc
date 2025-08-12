LLVM_SYS_181_PREFIX := "/usr/lib/llvm18/bin"

run: build-include
    cargo run

test: build-include
    cargo test

check: build-include
    cargo check

build: build-include
    cargo build --release

build-include:
    clang++ -c -fPIC include/llvm_bindings.cpp -o include/llvm_bindings.o $(llvm-config-18 --cxxflags)
    ar rcs include/libllvm_bindings.a include/llvm_bindings.o
