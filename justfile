LLVM_SYS_181_PREFIX := "$(llvm-config-18 --bindir)"

build: bindings
    cargo build --release

run *ARGS: bindings
    env EVSC_LIB_PATH="$(pwd)/lib" cargo run -- {{ARGS}}

gen-tests: bindings
    env EVSC_LIB_PATH="$(pwd)/lib" IS_DEV=1 cargo run gen_tests

test: bindings
    env EVSC_LIB_PATH="$(pwd)/lib" cargo test

check: bindings
    cargo check

clean:
    rm -rf include/*.a include/*.o tests/*-test.ll
    cargo clean

install:
    cargo install --path .

bindings:
    clang++ -c -fPIC include/llvm_bindings.cpp -o include/llvm_bindings.o $(llvm-config-18 --cxxflags)
    ar rcs include/libllvm_bindings.a include/llvm_bindings.o
