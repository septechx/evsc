mod common;

use common::it;
use oxic::errors::ErrorLevel;

/// Verifies that an empty program beginning with a shebang line compiles and runs with exit code 0.
///
/// # Examples
///
/// ```no_run
/// #[test]
/// fn can_compile_program_with_shebang() {
///     it(
///         "should compile an empty program with shebang successfully",
///         |ctx| {
///             ctx.add_source(
///                 r#"
///                 #!/usr/bin/env oxic
///                 pub fn main() void {}
///                 "#,
///             )
///             .compiles(true)
///             .execute(|res| {
///                 res.exit_code(0);
///             });
///         },
///     )
/// }
/// ```
#[test]
fn can_compile_program_with_shebang() {
    it(
        "should compile an empty program with shebang successfully",
        |ctx| {
            ctx.add_source(
                r#"
                #!/usr/bin/env oxic
                pub fn main() void {}
                "#,
            )
            .compiles(true)
            .execute(|res| {
                res.exit_code(0);
            });
        },
    )
}

#[test]
fn can_compile_empty_program() {
    it("should compile an empty program successfully", |ctx| {
        ctx.add_source("").compiles(true);
    })
}

#[test]
fn compiling_empty_program_emits_warning() {
    it(
        "should emit warning when compiling an empty program",
        |ctx| {
            ctx.add_source("")
                .compiles(true)
                .fail_on_level(ErrorLevel::Warning);
        },
    )
}

#[ignore]
#[test]
fn invalid_return_type_fails() {
    it("should error when main returns invalid type", |ctx| {
        ctx.add_source(
            r#"
                pub fn main() []u8 {
                    return []u8{1, 2, 3};
                }
                "#,
        )
        .compiles(false);
    })
}

#[test]
fn slice_literals() {
    it("should handle slice literals", |ctx| {
        ctx.add_source(
            r#"
            pub fn main() void {
                let s = []u8{1, 2, 3};
            }
        "#,
        )
        .compiles(true);
    })
}

#[ignore]
#[test]
fn internal_attribute_fails_in_user_code() {
    it(
        "should error when #[internal] is used in user code",
        |ctx| {
            ctx.add_source(
                r#"
                #[internal]
                pub fn internal_fn() isize {
                    return 0;
                }
                "#,
            )
            .compiles(false);
        },
    )
}

#[ignore]
#[test]
fn internal_attribute_on_struct_fails() {
    it(
        "should error when #[internal] is used on struct in user code",
        |ctx| {
            ctx.add_source(
                r#"
                #[internal]
                pub struct InternalStruct {
                    value: i32,
                }
                "#,
            )
            .compiles(false);
        },
    )
}

#[test]
fn test_attribute_works() {
    it("should parse attribute successfully", |ctx| {
        ctx.add_source(
            r#"
                #[test]
                pub fn main() isize {
                    return 42;
                }
                "#,
        )
        .compiles(true)
        .fail_on_level(ErrorLevel::Error)
        .execute(|res| {
            res.exit_code(42);
        });
    })
}

#[test]
fn attribute_with_arguments() {
    it("should parse attributes with arguments", |ctx| {
        ctx.add_source(
            r#"
                #[foo(bar, baz)]
                pub fn main() isize {
                    return 10;
                }
                "#,
        )
        .compiles(true)
        .fail_on_level(ErrorLevel::Error)
        .execute(|res| {
            res.exit_code(10);
        });
    })
}

#[test]
fn multiple_attributes() {
    it("should parse multiple attributes on function", |ctx| {
        ctx.add_source(
            r#"
                #[test]
                #[foo]
                pub fn main() isize {
                    return 5;
                }
                "#,
        )
        .compiles(true)
        .fail_on_level(ErrorLevel::Error)
        .execute(|res| {
            res.exit_code(5);
        });
    })
}

#[test]
fn main_fn_declaration() {
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
fn variable_declaration() {
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
fn multiple_variables_and_addition() {
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
fn struct_declaration_and_initialization() {
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
fn string_literals_and_slice_operations() {
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
fn import_and_print_function() {
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
fn struct_with_methods() {
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
fn sizeof_builtin() {
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

#[test]
fn main_function_return_void() {
    it("should exit with code 0 successfully", |ctx| {
        ctx.add_source(
            r#"
            pub fn main() void {}
        "#,
        )
        .compiles(true)
        .ir_eq("11.ll")
        .execute(|res| {
            res.exit_code(0);
        });
    })
}