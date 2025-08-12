; ModuleID = '04-test.evsc'
source_filename = "04-test.evsc"

define i32 @main() {
entry:
  %a = alloca i32, align 4
  store i32 1, ptr %a, align 4
  %b = alloca i32, align 4
  store i32 2, ptr %b, align 4
  %left = load i32, ptr %a, align 4
  %right = load i32, ptr %b, align 4
  %sumtmp = add i32 %left, %right
  %c = alloca i32, align 4
  store i32 %sumtmp, ptr %c, align 4
  %load_ptr = load i32, ptr %c, align 4
  ret i32 %load_ptr
}
