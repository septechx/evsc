; ModuleID = 'main'
source_filename = "main"

%Module_std = type { ptr }
%Slice = type { ptr, i64 }

@std = private constant %Module_std { ptr @print }
@str = private constant [11 x i8] c"Hello world"
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define i64 @main() {
entry:
  %loaded_struct = load %Module_std, ptr @std, align 8
  %field = extractvalue %Module_std %loaded_struct, 0
  %calltmp = call ptr %field(%Slice { ptr @str, i64 11 })
  ret i32 0
}

define void @print(%Slice %str) {
entry:
  %str1 = alloca %Slice, align 8
  store %Slice %str, ptr %str1, align 8
  %loaded_struct = load %Slice, ptr %str1, align 8
  %field = extractvalue %Slice %loaded_struct, 0
  %loaded_struct2 = load %Slice, ptr %str1, align 8
  %field3 = extractvalue %Slice %loaded_struct2, 1
  call void asm sideeffect inteldialect "mov rax, 1\0Amov rdi, 1\0Amov rsi, $0\0Amov rdx, $1\0Asyscall", "r,r,~{rax},~{rdi},~{rsi},~{rdx},~{rcx},~{r11},~{cc}"(ptr %field, i64 %field3)
  ret void
}
