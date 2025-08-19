# evscc

Evscc is a compiler for the Evsc programming language.

```zig
static std = @import("std");

pub fn main() i32 {
    std.print("Hello world");

    return 0;
}
```

## Requirements

**Building:**

- Rust
- Clang
- LLVM 18.1.x
- just

**Compiling:**

- LLVM 18.1.x
- A linker

## Building

```sh
just build
```

## Testing

```sh
just test
```

## Generating tests

Tests are already included in the repo, but if you want to add a new one, or make changes that break the tests, you can generate new tests with:

```sh
just gen-tests
```

## Usage

Run `evscc --help` for more information.

### Compiling

```sh
evscc file.evsc
```

By default, `libc` will be linked, this behavior can be disabled with the `--no-libc` flag. The standard library does not depend on libc, so you can use it without linking it.

### Standard library

The compiler expects the standard library to be in `/usr/local/share/evsc/lib/` or `/opt/evsc/lib/`. The `EVSC_STD_LIB_PATH` environment variable can be used to override this.

The standard library is only needed to be available at compile time, the compiler will include the needed functions in the final binary.

## Notes

- The compiler is still in development, so expect some bugs and missing features.
- The name is temporary, and will be changed to something better.
