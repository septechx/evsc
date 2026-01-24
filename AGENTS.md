# Running the tests

Either use `just test` or `env OXI_ROOT=$(pwd) cargo test`. Do not run the tests unless told to, you can run the lint instead.

# Running the lint

Use `just lint`

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
