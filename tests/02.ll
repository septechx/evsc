; ModuleID = '02-test.evsc'
source_filename = "02-test.evsc"

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define void @main() {
entry:
  %a = alloca i32, align 4
  store i32 1, ptr %a, align 4
  ret void
}
