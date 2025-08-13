set positional-arguments

LLVM_SYS_181_PREFIX := "$(llvm-config-18 --bindir)"

@run file: build-include
    env EVSC_STD_LIB_PATH="$(pwd)/lib/std" cargo run $1

@gen-tests: build-include
    env EVSC_STD_LIB_PATH="$(pwd)/lib/std" IS_DEV=1 cargo run gen_tests

@test: build-include
    env EVSC_STD_LIB_PATH="$(pwd)/lib/std" cargo test

@check: build-include
    cargo check

@build: build-include
    cargo build --release

@clean:
    rm -rf include/libllvm_bindings.a include/llvm_bindings.o
    rm -rf tests/*-test.ll
    cargo clean


@build-include:
    clang++ -c -fPIC include/llvm_bindings.cpp -o include/llvm_bindings.o $(llvm-config-18 --cxxflags)
    ar rcs include/libllvm_bindings.a include/llvm_bindings.o
