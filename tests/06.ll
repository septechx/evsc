; ModuleID = '06-test.evsc'
source_filename = "06-test.evsc"

%Slice = type { ptr, i64 }

@str = private constant [12 x i8] c"Hello world!"
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define i64 @main() {
entry:
  %s = alloca %Slice, align 8
  store %Slice { ptr @str, i64 12 }, ptr %s, align 8
  %load_ptr = load %Slice, ptr %s, align 8
  %field_ptr = getelementptr inbounds %Slice, ptr %s, i32 0, i32 0
  %load_ptr1 = load ptr, ptr %field_ptr, align 8
  %ptr = alloca ptr, align 8
  store ptr %load_ptr1, ptr %ptr, align 8
  %load_ptr2 = load %Slice, ptr %s, align 8
  %field_ptr3 = getelementptr inbounds %Slice, ptr %s, i32 0, i32 1
  %load_ptr4 = load i64, ptr %field_ptr3, align 4
  %len = alloca i64, align 8
  store i64 %load_ptr4, ptr %len, align 4
  %load_ptr5 = load i64, ptr %len, align 4
  ret i64 %load_ptr5
}
