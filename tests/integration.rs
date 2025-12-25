mod common;

use common::it;

#[test]
fn test_main_fn_declaration() {
    it("should compile a main function declaration", |ctx| {
        ctx.add_source(
            r#"
            pub fn main() isize {
                return 0;
            }
        "#,
        )
        .compiles(true)
        .ir_eq("01.ll")
        .execute(&|res| {
            res.exit_code(0);
        });
    })
}
