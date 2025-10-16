use super::{LLVMContext, LLVMModule};
use crate::ir::OperationRef;
use qwerty_mlir_sys::{mlirTransferModuleFlags, mlirTranslateModuleToLLVMIR};

pub fn translate_module<'c, 'l>(
    module: OperationRef<'c, '_>,
    llvm_ctx: &'l LLVMContext,
) -> Option<LLVMModule<'l>> {
    unsafe {
        let ret = LLVMModule::from_option_raw(mlirTranslateModuleToLLVMIR(
            module.to_raw(),
            llvm_ctx.to_raw(),
        ));
        if let Some(llvm_mod) = &ret {
            mlirTransferModuleFlags(module.to_raw(), llvm_mod.to_raw());
        }
        ret
    }
}
