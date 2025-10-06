; ModuleID = '06-test.evsc'
source_filename = "06-test.evsc"

%Slice = type { ptr, i64 }

@str = private constant [12 x i8] c"Hello world!"
@llvm.global_ctors = appending global [1 x { i64, ptr, ptr }] [{ i64, ptr, ptr } { i64 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define i64 @main() {
entry:
  %s = alloca %Slice, align 8
  store %Slice { ptr @str, i64 12 }, ptr %s, align 8
  %load_ptr = load %Slice, ptr %s, align 8
  %loaded_struct = load %Slice, ptr %s, align 8
  %field = extractvalue %Slice %loaded_struct, 0
  %ptr = alloca ptr, align 8
  store ptr %field, ptr %ptr, align 8
  %load_ptr1 = load %Slice, ptr %s, align 8
  %loaded_struct2 = load %Slice, ptr %s, align 8
  %field3 = extractvalue %Slice %loaded_struct2, 1
  %len = alloca i64, align 8
  store i64 %field3, ptr %len, align 4
  %load_ptr4 = load i64, ptr %len, align 4
  ret i64 %load_ptr4
}
