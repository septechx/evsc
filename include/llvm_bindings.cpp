#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

extern "C" {
LLVMTypeRef LLVMExtCreateNamedStruct(
  LLVMContextRef ctx,
  LLVMTypeRef* elements,
  unsigned count,
  const char* name,
  bool isPacked
) {
  llvm::ArrayRef<llvm::Type*> arr(
    reinterpret_cast<llvm::Type**>(elements),
    count
  );
  return reinterpret_cast<LLVMTypeRef>(
    llvm::StructType::create(
      *reinterpret_cast<llvm::LLVMContext*>(ctx),
      arr,
      name,
      isPacked
    )
  );
}
}
