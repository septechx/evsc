; ModuleID = '02.evsc'
source_filename = "02.evsc"

define void @main() {
entry:
  %a = alloca i32, align 4
  store i32 1, ptr %a, align 4
  ret void
}
