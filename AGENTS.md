# Running the tests

Either use `just test` or `env OXI_ROOT=$(pwd) cargo test`.

# Language syntax

## Arrays

```
let a = []u8{1, 2, 3};
```

## Functions

```
fn add(a: u32, b: u32)  u32 {
    return a + b;
}
```
