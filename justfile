LLVM_SYS_211_PREFIX := "$(llvm-config --bindir)"

build:
    cargo build --release

run *ARGS:
    env OXI_ROOT="$(pwd)" cargo run -- {{ARGS}}

test:
    env OXI_ROOT="$(pwd)" cargo test

check:
    cargo check

clean:
    cargo clean

install: build
    sudo install -D -m755 target/release/oxic /usr/bin/oxic
    sudo rsync -a --delete lib/oxi/ /usr/lib/oxi

lint:
    cargo clippy --all-targets --all-features -- -Dwarnings
