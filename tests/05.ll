; ModuleID = '05-test.evsc'
source_filename = "05-test.evsc"

%Foo = type { i32 }

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__module_init, ptr null }]

define void @__module_init() {
entry:
  ret void
}

define void @main() {
entry:
  %inst_Foo = alloca %Foo, align 8
  %a_ptr = getelementptr inbounds %Foo, ptr %inst_Foo, i32 0, i32 0
  store i32 1, ptr %a_ptr, align 4
  %load_inst = load %Foo, ptr %inst_Foo, align 4
  %foo = alloca %Foo, align 8
  store %Foo %load_inst, ptr %foo, align 4
  ret void
}
