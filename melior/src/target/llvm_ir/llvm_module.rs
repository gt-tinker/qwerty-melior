use crate::{target::llvm_ir::LLVMContext, Error};
use qwerty_mlir_sys::{LLVMDisposeMessage, LLVMModuleRef, LLVMPrintModuleToFile};
use std::{
    ffi::{CStr, CString},
    marker::PhantomData,
    path::Path,
};

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

    pub fn print_to_file<P: AsRef<Path>>(&self, filename: P) -> Result<(), Error> {
        // This is NOT null terminated
        let filename_byte_slice = filename.as_ref().as_os_str().as_encoded_bytes();
        // ...but this is
        let path_str = CString::new(filename_byte_slice)
            .map_err(|nul_err| Error::PrintLLVMModule(nul_err.to_string()))?;

        unsafe {
            let mut error_message: *mut i8 = std::ptr::null_mut();
            let ret = LLVMPrintModuleToFile(
                self.to_raw(),
                path_str.as_ptr(),
                &mut error_message as *mut _,
            );
            // 0 is success, 1 is failure
            if ret == 0 {
                assert!(error_message.is_null(), "Error message for success?");
                Ok(())
            } else {
                let err = match CStr::from_ptr(error_message).to_str() {
                    Ok(msg) => Error::PrintLLVMModule(msg.to_string()),
                    Err(utf8_err) => Error::Utf8(utf8_err),
                };

                LLVMDisposeMessage(error_message);

                Err(err)
            }
        }
    }
}
