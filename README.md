<div align="center">
  <picture>
    <img alt="oxi" src="assets/oxi_logo.png" width="50%">
  </picture>
</div>

---

![CI status](https://github.com/septechx/oxi/actions/workflows/linux.yml/badge.svg)

Oxic is a compiler for the Oxi programming language.

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
- Mold/lld/gold/ld

## Installation

```sh
just install
```

## Running tests

```sh
just test
```

## Usage

Run `oxic --help` for more information.

### Compiling

```sh
oxic file.oxi
```

### Standard library

The compiler expects the standard library under `<root>/lib/oxi/` (e.g. `/usr/lib/oxi/` when installed to `/usr/bin/oxic`). The `OXI_LIB_PATH` environment variable can be used to override this.
The standard library is only needed to be available at compile time, the compiler will include the needed functions in the final binary.

## Notes

- The compiler is still in development, expect bugs and missing features.
