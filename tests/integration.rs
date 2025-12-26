mod common;

use common::it;

#[test]
fn test_01_main_fn_declaration() {
    it(
        "should compile a main function declaration correctly",
        |ctx| {
            ctx.add_source(
                r#"
            pub fn main() isize {
                return 0;
            }
        "#,
            )
            .compiles(true)
            .ir_eq("01.ll")
            .execute(|res| {
                res.exit_code(0);
            });
        },
    )
}

#[test]
fn test_02_variable_declaration() {
    it(
        "should handle variable declarations in main function",
        |ctx| {
            ctx.add_source(
                r#"
            pub fn main() isize {
                let a = 2;
                return a;
            }
        "#,
            )
            .compiles(true)
            .ir_eq("02.ll")
            .execute(|res| {
                res.exit_code(2);
            });
        },
    )
}

#[test]
fn test_03_multiple_variables_and_addition() {
    it(
        "should handle multiple variables and addition operations",
        |ctx| {
            ctx.add_source(
                r#"
            pub fn main() isize {
                let a = 1;
                let b = 2;
                let c = a + b;
                return c;
            }
        "#,
            )
            .compiles(true)
            .ir_eq("03.ll")
            .execute(|res| {
                res.exit_code(3);
            });
        },
    )
}

#[test]
fn test_05_struct_declaration_and_initialization() {
    it(
        "should handle struct declaration and initialization",
        |ctx| {
            ctx.add_source(
                r#"
            struct Foo {
                a: i32,
            }

            pub fn main() isize {
                let foo = Foo {
                    a: 1,
                };

                return foo.a;
            }
        "#,
            )
            .compiles(true)
            .ir_eq("05.ll")
            .execute(|res| {
                res.exit_code(1);
            });
        },
    )
}

#[test]
fn test_06_string_literals_and_slice_operations() {
    it(
        "should handle string literals and slice operations",
        |ctx| {
            ctx.add_source(
                r#"
            pub fn main() i64 {
                let s = "Hello world!";
                let ptr = s.ptr;
                let len = s.len;
                return len;
            }
        "#,
            )
            .compiles(true)
            .ir_eq("06.ll")
            .execute(|res| {
                res.exit_code(12);
            });
        },
    )
}

#[test]
fn test_07_import_and_print_function() {
    it("should handle import and std print function", |ctx| {
        ctx.add_source(
            r#"
            static std = @import("std");

            pub fn main() isize {
                std.print("Hello world");

                return 0;
            }
        "#,
        )
        .compiles(true)
        .ir_eq("07.ll")
        .execute(|res| {
            res.exit_code(0).stdout("Hello world");
        });
    })
}

#[test]
fn test_08_struct_with_methods() {
    it("should handle struct declaration with methods", |ctx| {
        ctx.add_source(
            r#"
            struct Foo {
                a: i32,

                pub static fn bar(a: i32, b: i32) i32 {
                    return a - b;
                }
            }

            pub fn main() isize {
                let foo = Foo {
                    a: 1,
                };

                return foo.a - foo.bar(2, 1);
            }
        "#,
        )
        .compiles(true)
        .ir_eq("08.ll")
        .execute(|res| {
            res.exit_code(0);
        });
    })
}

#[test]
fn test_09_return_string_literal() {
    it("should handle returning string literals", |ctx| {
        ctx.add_source(
            r#"
            pub fn main() []const u8 {
                return "Hello world!\n";
            }
        "#,
        )
        .compiles(true)
        .ir_eq("09.ll");
    })
}

#[test]
fn test_10_sizeof_builtin() {
    it("should handle @sizeof builtin operator", |ctx| {
        ctx.add_source(
            r#"
            pub fn main() isize {
                return @sizeof($u16);
            }
        "#,
        )
        .compiles(true)
        .ir_eq("10.ll")
        .execute(|res| {
            res.exit_code(2);
        });
    })
}
