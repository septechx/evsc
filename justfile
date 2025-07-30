LLVM_SYS_181_PREFIX := "/usr/lib/llvm18/bin"

run:
    cargo run
    lli-18 _test/01.ll

check:
    cargo check

build:
    cargo build --release

