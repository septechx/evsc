; ModuleID = 'main'
source_filename = "main"

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define i64 @main() {
entry:
  %a = alloca i64, align 8
  store i64 2, ptr %a, align 4
  %load_ptr = load i64, ptr %a, align 4
  ret i64 %load_ptr
}
