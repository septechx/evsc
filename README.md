# evscc

Evscc is a compiler for the Evsc programming language.

```zig
static std = @import("std");

pub fn main() isize {
    std.print("Hello world\n");

    return 0;
}
```

## Requirements

**Building:**

- Rust
- Clang
- LLVM 21.1
- just

**Usage:**

- LLVM 21.1
- libclang >=10.0

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

### Standard library

The compiler expects the standard library to be in `/usr/local/share/evsc/lib/` or `/opt/evsc/lib/`. The `EVSC_LIB_PATH` environment variable can be used to override this.

The standard library is only needed to be available at compile time, the compiler will include the needed functions in the final binary.

## Notes

- The compiler is still in development, expect bugs and missing features.
- The name is temporary, and will be changed to something better.
