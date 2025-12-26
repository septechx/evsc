; ModuleID = 'main'
source_filename = "main"

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define i64 @main() {
entry:
  ret i64 ptrtoint (ptr getelementptr (i16, ptr null, i32 1) to i64)
}
