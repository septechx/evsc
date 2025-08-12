; ModuleID = '06.evsc'
source_filename = "06.evsc"

%Slice = type { ptr, i64 }

@str = private constant [11 x i8] c"Hello world"

define i32 @main() {
entry:
  %s = alloca %Slice, align 8
  store %Slice { ptr @str, i64 11 }, ptr %s, align 8
  %field_ptr = getelementptr inbounds %Slice, ptr %s, i32 0, i32 0
  %ptr = alloca ptr, align 8
  store ptr %field_ptr, ptr %ptr, align 8
  %field_ptr1 = getelementptr inbounds %Slice, ptr %s, i32 0, i32 1
  %len = alloca ptr, align 8
  store ptr %field_ptr1, ptr %len, align 8
  %ret = load ptr, ptr %len, align 8
  ret ptr %ret
}
