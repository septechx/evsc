; ModuleID = '07-test.evsc'
source_filename = "07-test.evsc"

%Module_std = type { ptr }
%Slice = type { ptr, i64 }

@inst_Module_std = private constant %Module_std { ptr @print }
@std = private global %Module_std zeroinitializer
@str = private constant [11 x i8] c"Hello world"
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  %load_ptr = load %Module_std, ptr @inst_Module_std, align 8
  store %Module_std %load_ptr, ptr @std, align 8
  ret void
}

define i32 @main() {
entry:
  %load_ptr = load %Module_std, ptr @std, align 8
  %load_func_ptr = load ptr, ptr @std, align 8
  %calltmp = call ptr %load_func_ptr(%Slice { ptr @str, i64 11 })
  ret i32 0
}

define void @print(%Slice %str) {
entry:
  %str1 = alloca %Slice, align 8
  store %Slice %str, ptr %str1, align 8
  %load_ptr = load %Slice, ptr %str1, align 8
  %field_ptr = getelementptr inbounds %Slice, ptr %str1, i32 0, i32 0
  %load_ptr2 = load ptr, ptr %field_ptr, align 8
  %load_ptr3 = load %Slice, ptr %str1, align 8
  %field_ptr4 = getelementptr inbounds %Slice, ptr %str1, i32 0, i32 1
  %load_ptr5 = load i64, ptr %field_ptr4, align 4
  call void asm sideeffect inteldialect "\0A      mov rax, 1\0A      mov rdi, 1\0A      mov rsi, $0\0A      mov rdx, $1\0A      syscall\0A    ", "r,r,~{rax},~{rdi},~{rsi},~{rdx},~{rcx},~{r11},~{cc}"(ptr %load_ptr2, i64 %load_ptr5)
  ret void
}
