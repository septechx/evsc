mod common;

use common::it;
use oxic::errors::ErrorLevel;

// TODO: This is supported by the parser, but not by the codegen
#[ignore]
#[test]
fn can_compile_interface_declaration() {
    it(|ctx| {
        ctx.add_source(
            r#"
            interface Foo {
                fn bar() void,
            }
            "#,
        )
        .compiles(true);
    })
}

#[test]
fn can_compile_nested_block_with_implicit_returns() {
    it(|ctx| {
        ctx.add_source(
            r#"
            pub fn main() void {
                {
                    {
                        {}
                    }
                }
            }
            "#,
        )
        .compiles(true);
    })
}

#[test]
fn can_compile_simple_nested_block() {
    it(|ctx| {
        ctx.add_source(
            r#"
            pub fn main() void {
                {
                    {
                        {};
                    };
                };
            }
            "#,
        )
        .compiles(true);
    })
}

#[test]
fn duplicate_struct_property_fails() {
    it(|ctx| {
        ctx.add_source(
            r#"
            struct Foo {
                a: i32,
                a: i32,
            }

            pub fn main() void {}
            "#,
        )
        .compiles(false);
    })
}

#[test]
fn can_compile_program_with_shebang() {
    it(|ctx| {
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
    })
}

#[test]
fn can_compile_empty_program() {
    it(|ctx| {
        ctx.add_source("").compiles(true);
    })
}

#[test]
fn compiling_empty_program_emits_warning() {
    it(|ctx| {
        ctx.add_source("")
            .compiles(true)
            .fail_on_level(ErrorLevel::Warning);
    })
}

#[ignore]
#[test]
fn invalid_return_type_fails() {
    it(|ctx| {
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
    it(|ctx| {
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
    it(|ctx| {
        ctx.add_source(
            r#"
            #[internal]
            pub fn internal_fn() isize {
                return 0;
            }
            "#,
        )
        .compiles(false);
    })
}

#[ignore]
#[test]
fn internal_attribute_on_struct_fails() {
    it(|ctx| {
        ctx.add_source(
            r#"
            #[internal]
            pub struct InternalStruct {
                value: i32,
            }
            "#,
        )
        .compiles(false);
    })
}

#[test]
fn test_attribute_works() {
    it(|ctx| {
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
    it(|ctx| {
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
    it(|ctx| {
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
    it(|ctx| {
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
    })
}

#[test]
fn variable_declaration() {
    it(|ctx| {
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
    })
}

#[test]
fn multiple_variables_and_addition() {
    it(|ctx| {
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
    })
}

#[test]
fn struct_declaration_and_initialization() {
    it(|ctx| {
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
    })
}

#[test]
fn string_literals_and_slice_operations() {
    it(|ctx| {
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
    })
}

#[ignore]
#[test]
fn import_and_print_function() {
    it(|ctx| {
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
    it(|ctx| {
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
    it(|ctx| {
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
    it(|ctx| {
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

#[test]
fn integer_literals() {
    it(|t| {
        t.add_source(
            r#"
            fn main() i32 {
                let a: i32 = 10;
                let b: i32 = -5;
                return a + b;
            }
            "#,
        )
        .execute(|res| {
            res.exit_code(5);
        });
    });
}

#[test]
fn boolean_literals() {
    it(|t| {
        t.add_source(
            r#"
            fn main() bool {
                let a: bool = true;
                return a;
            }
            "#,
        )
        .execute(|res| {
            res.exit_code(1);
        });
    });
}

#[test]
fn char_literals() {
    it(|t| {
        t.add_source(
            r#"
            fn main() u8 {
                let a: u8 = 'a';
                return a;
            }
            "#,
        )
        .execute(|res| {
            res.exit_code(97);
        });
    });
}

#[test]
fn string_literals() {
    it(|t| {
        t.add_source(
            r#"
            fn main() isize {
                let s: []u8 = "hello";
                return s.len;
            }
            "#,
        )
        .execute(|res| {
            res.exit_code(5);
        });
    });
}

#[test]
fn struct_shorthand_initialization() {
    it(|ctx| {
        ctx.add_source(
            r#"
            struct Foo {
                x: i32,
                y: i32,
            }

            pub fn main() isize {
                let x = 10;
                let foo = Foo {
                    x,
                    y: 20,
                };

                return foo.x + foo.y;
            }
            "#,
        )
        .compiles(true)
        .execute(|res| {
            res.exit_code(30);
        });
    })
}

#[test]
fn float_literals() {
    it(|t| {
        t.add_source(
            r#"
            fn main() i32 {
                let a: f64 = 1.5;
                let b: f64 = 2.5;
                // We don't have float comparison yet, so we just check if it compiles 
                // and return a dummy value
                return 1;
            }
            "#,
        )
        .execute(|res| {
            res.exit_code(1);
        });
    });
}
