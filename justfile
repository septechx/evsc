LLVM_SYS_211_PREFIX := "$(llvm-config --bindir)"

build: bindings
    cargo build --release

run *ARGS: bindings
    env EVSC_ROOT="$(pwd)" cargo run -- {{ARGS}}

test-debug: bindings
    env EVSC_DEBUG_TESTS=1 EVSC_ROOT="$(pwd)" cargo test

test: bindings
    env EVSC_ROOT="$(pwd)" cargo test

check: bindings
    cargo check

clean:
    rm -rf include/*.a include/*.o tests/*-test.ll
    cargo clean

install: build
    sudo install -D -m755 target/release/evscc /usr/bin/evscc
    sudo rsync -a --delete lib/evsc/ /usr/lib/evsc

lint:
    cargo clippy --all-targets --all-features -- -Dwarnings

bindings:
    clang++ -c -fPIC include/llvm_bindings.cpp -o include/llvm_bindings.o $(llvm-config --cxxflags)
    ar rcs include/libllvm_bindings.a include/llvm_bindings.o
