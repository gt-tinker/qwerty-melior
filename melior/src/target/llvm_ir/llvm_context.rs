use qwerty_mlir_sys::{LLVMContextCreate, LLVMContextDispose, LLVMContextRef};

#[derive(Debug)]
pub struct LLVMContext {
    raw: LLVMContextRef,
}

impl LLVMContext {
    pub fn new() -> Self {
        Self {
            raw: unsafe { LLVMContextCreate() },
        }
    }

    pub const fn to_raw(&self) -> LLVMContextRef {
        self.raw
    }
}

impl Drop for LLVMContext {
    fn drop(&mut self) {
        unsafe { LLVMContextDispose(self.raw) };
    }
}
