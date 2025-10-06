; ModuleID = '07-test.evsc'
source_filename = "07-test.evsc"

%Module_std = type { ptr }
%Slice = type { ptr, i64 }

@inst_Module_std = private constant %Module_std { ptr @print }
@std = private global %Module_std zeroinitializer
@str = private constant [11 x i8] c"Hello world"
@llvm.global_ctors = appending global [1 x { i64, ptr, ptr }] [{ i64, ptr, ptr } { i64 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  %load_ptr = load %Module_std, ptr @inst_Module_std, align 8
  store %Module_std %load_ptr, ptr @std, align 8
  ret void
}

define i32 @main() {
entry:
  %load_ptr = load %Module_std, ptr @std, align 8
  %loaded_struct = load %Module_std, ptr @std, align 8
  %field = extractvalue %Module_std %loaded_struct, 0
  %calltmp = call ptr %field(%Slice { ptr @str, i64 11 })
  ret i32 0
}

define void @print(%Slice %str) {
entry:
  %str1 = alloca %Slice, align 8
  store %Slice %str, ptr %str1, align 8
  %load_ptr = load %Slice, ptr %str1, align 8
  %loaded_struct = load %Slice, ptr %str1, align 8
  %field = extractvalue %Slice %loaded_struct, 0
  %load_ptr2 = load %Slice, ptr %str1, align 8
  %loaded_struct3 = load %Slice, ptr %str1, align 8
  %field4 = extractvalue %Slice %loaded_struct3, 1
  %asm = call i64 asm sideeffect inteldialect "mov rax, 1\0Amov rdi, 1\0Amov rsi, $0\0Amov rdx, $1\0Asyscall", "r,r,~{rax},~{rdi},~{rsi},~{rdx},~{rcx},~{r11},~{cc}"(ptr %field, i64 %field4)
  ret void
}
