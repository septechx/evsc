; ModuleID = '04-test.evsc'
source_filename = "04-test.evsc"

@llvm.global_ctors = appending global [1 x { i64, ptr, ptr }] [{ i64, ptr, ptr } { i64 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define i32 @main() {
entry:
  %a = alloca i32, align 4
  store i32 1, ptr %a, align 4
  %b = alloca i32, align 4
  store i32 2, ptr %b, align 4
  %load_ptr = load i32, ptr %a, align 4
  %load_ptr1 = load i32, ptr %b, align 4
  %sumtmp = add i32 %load_ptr, %load_ptr1
  %c = alloca i32, align 4
  store i32 %sumtmp, ptr %c, align 4
  %load_ptr2 = load i32, ptr %c, align 4
  ret i32 %load_ptr2
}
