use super::LLVMContext;
use qwerty_mlir_sys::LLVMModuleRef;
use std::marker::PhantomData;

#[derive(Debug)]
pub struct LLVMModule<'l> {
    raw: LLVMModuleRef,
    _llvm_context: PhantomData<&'l LLVMContext>,
}

impl<'l> LLVMModule<'l> {
    pub unsafe fn from_raw(raw: LLVMModuleRef) -> Self {
        Self {
            raw,
            _llvm_context: PhantomData,
        }
    }

    pub unsafe fn from_option_raw(raw: LLVMModuleRef) -> Option<Self> {
        if raw.is_null() {
            None
        } else {
            Some(Self::from_raw(raw))
        }
    }

    pub const fn to_raw(&self) -> LLVMModuleRef {
        self.raw
    }

    pub const fn into_raw(self) -> LLVMModuleRef {
        self.raw
    }
}
