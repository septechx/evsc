set positional-arguments

LLVM_SYS_181_PREFIX := "$(llvm-config-18 --bindir)"

@build: build-include
    cargo build --release

@run file: build-include
    env EVSC_STD_LIB_PATH="$(pwd)/lib/std" cargo run $1

@gen-tests: build-include
    env EVSC_STD_LIB_PATH="$(pwd)/lib/std" IS_DEV=1 cargo run gen_tests

@test: build-include
    env EVSC_STD_LIB_PATH="$(pwd)/lib/std" cargo test

@check: build-include
    cargo check

@clean:
    rm -rf include/*.a include/*.o tests/*-test.ll
    cargo clean


@build-include:
    clang++ -c -fPIC include/llvm_bindings.cpp -o include/llvm_bindings.o $(llvm-config-18 --cxxflags)
    ar rcs include/libllvm_bindings.a include/llvm_bindings.o
