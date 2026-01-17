LLVM_SYS_211_PREFIX := "$(llvm-config --bindir)"

build: bindings
    cargo build --release

run *ARGS: bindings
    env OXI_ROOT="$(pwd)" cargo run -- {{ARGS}}

test-debug: bindings
    env OXI_DEBUG_TESTS=1 OXI_ROOT="$(pwd)" cargo test

test: bindings
    env OXI_ROOT="$(pwd)" cargo test

check: bindings
    cargo check

clean:
    rm -rf include/*.a include/*.o tests/*-test.ll
    cargo clean

install: build
    sudo install -D -m755 target/release/oxic /usr/bin/oxic
    sudo rsync -a --delete lib/oxi/ /usr/lib/oxi

lint:
    cargo clippy --all-targets --all-features -- -Dwarnings

bindings:
    clang++ -c -fPIC include/llvm_bindings.cpp -o include/llvm_bindings.o $(llvm-config --cxxflags)
    ar rcs include/libllvm_bindings.a include/llvm_bindings.o
