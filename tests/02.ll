; ModuleID = 'main'
source_filename = "main"

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define i64 @main() {
entry:
  %a = alloca i32, align 4
  store i32 2, ptr %a, align 4
  %load_ptr = load i32, ptr %a, align 4
  ret i32 %load_ptr
}
