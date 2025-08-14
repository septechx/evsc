; ModuleID = '08-test.evsc'
source_filename = "08-test.evsc"

%Foo = type { i32 }

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define i32 @Foo_bar(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, ptr %b2, align 4
  %load_ptr = load i32, ptr %a1, align 4
  %load_ptr3 = load i32, ptr %b2, align 4
  %subtmp = sub i32 %load_ptr, %load_ptr3
  ret i32 %subtmp
}

define i32 @main() {
entry:
  %inst_Foo = alloca %Foo, align 8
  %a_ptr = getelementptr inbounds %Foo, ptr %inst_Foo, i32 0, i32 0
  store i32 1, ptr %a_ptr, align 4
  %load_inst = load %Foo, ptr %inst_Foo, align 4
  %foo = alloca %Foo, align 8
  store %Foo %load_inst, ptr %foo, align 4
  %load_ptr = load %Foo, ptr %foo, align 4
  %field_ptr = getelementptr inbounds %Foo, ptr %foo, i32 0, i32 0
  %load_ptr1 = load %Foo, ptr %foo, align 4
  %calltmp = call i32 @Foo_bar(i32 2, i32 1)
  %load_ptr2 = load i32, ptr %field_ptr, align 4
  %subtmp = sub i32 %load_ptr2, %calltmp
  ret i32 %subtmp
}
