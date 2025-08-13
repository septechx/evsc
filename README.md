# evscc

Evscc is a compiler for the Evsc programming language.

## Building

Requirements:

- Rust
- Clang
- LLVM 18.1.x
- just

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
