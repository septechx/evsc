; ModuleID = '06-test.evsc'
source_filename = "06-test.evsc"

%Slice = type { ptr, i64 }

@str = private constant [12 x i8] c"Hello world!"

define i64 @main() {
entry:
  %s = alloca %Slice, align 8
  store %Slice { ptr @str, i64 12 }, ptr %s, align 8
  %field_ptr = getelementptr inbounds %Slice, ptr %s, i32 0, i32 0
  %load_ptr = load ptr, ptr %field_ptr, align 8
  %ptr = alloca ptr, align 8
  store ptr %load_ptr, ptr %ptr, align 8
  %field_ptr1 = getelementptr inbounds %Slice, ptr %s, i32 0, i32 1
  %load_ptr2 = load i64, ptr %field_ptr1, align 4
  %len = alloca i64, align 8
  store i64 %load_ptr2, ptr %len, align 4
  %load_ptr3 = load i64, ptr %len, align 4
  ret i64 %load_ptr3
}
