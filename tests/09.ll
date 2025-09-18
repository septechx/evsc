; ModuleID = '09-test.evsc'
source_filename = "09-test.evsc"

%Slice = type { ptr, i64 }

@str = private constant [13 x i8] c"Hello world!\0A"
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define %Slice @main() {
entry:
  ret %Slice { ptr @str, i64 13 }
}
