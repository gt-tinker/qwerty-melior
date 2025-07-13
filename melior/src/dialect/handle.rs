use super::DialectRegistry;
use crate::{context::Context, dialect::Dialect, string_ref::StringRef};
use qwerty_mlir_sys::{
    mlirDialectHandleGetNamespace, mlirDialectHandleInsertDialect, mlirDialectHandleLoadDialect,
    mlirDialectHandleRegisterDialect, mlirGetDialectHandle__arith__, mlirGetDialectHandle__cf__,
    mlirGetDialectHandle__func__, mlirGetDialectHandle__llvm__, mlirGetDialectHandle__math__,
    mlirGetDialectHandle__qcirc__, mlirGetDialectHandle__qwerty__, mlirGetDialectHandle__scf__,
    MlirDialectHandle,
};

/// A dialect handle.
#[derive(Clone, Copy, Debug)]
pub struct DialectHandle {
    raw: MlirDialectHandle,
}

impl DialectHandle {
    /// Creates a `cf` dialect handle.
    pub fn cf() -> Self {
        unsafe { Self::from_raw(mlirGetDialectHandle__cf__()) }
    }

    /// Creates a `func` dialect handle.
    pub fn func() -> Self {
        unsafe { Self::from_raw(mlirGetDialectHandle__func__()) }
    }

    /// Creates a `llvm` dialect handle.
    pub fn llvm() -> Self {
        unsafe { Self::from_raw(mlirGetDialectHandle__llvm__()) }
    }

    /// Creates a `scf` dialect handle.
    pub fn scf() -> Self {
        unsafe { Self::from_raw(mlirGetDialectHandle__scf__()) }
    }

    /// Creates a `arith` dialect handle.
    pub fn arith() -> Self {
        unsafe { Self::from_raw(mlirGetDialectHandle__arith__()) }
    }

    /// Creates a `math` dialect handle.
    pub fn math() -> Self {
        unsafe { Self::from_raw(mlirGetDialectHandle__math__()) }
    }

    /// Creates a `qcirc` dialect handle.
    pub fn qcirc() -> Self {
        unsafe { Self::from_raw(mlirGetDialectHandle__qcirc__()) }
    }

    /// Creates a `qwerty` dialect handle.
    pub fn qwerty() -> Self {
        unsafe { Self::from_raw(mlirGetDialectHandle__qwerty__()) }
    }

    /// Returns a namespace.
    pub fn namespace(&self) -> StringRef<'_> {
        unsafe { StringRef::from_raw(mlirDialectHandleGetNamespace(self.raw)) }
    }

    /// Inserts a dialect into a dialect registry.
    pub fn insert_dialect(&self, registry: &DialectRegistry) {
        unsafe { mlirDialectHandleInsertDialect(self.raw, registry.to_raw()) }
    }

    /// Loads a dialect into a context.
    pub fn load_dialect<'c>(&self, context: &'c Context) -> Dialect<'c> {
        unsafe { Dialect::from_raw(mlirDialectHandleLoadDialect(self.raw, context.to_raw())) }
    }

    /// Registers a dialect into a context.
    pub fn register_dialect(&self, context: &Context) {
        unsafe { mlirDialectHandleRegisterDialect(self.raw, context.to_raw()) }
    }

    /// Creates a dialect handle from a raw object.
    ///
    /// # Safety
    ///
    /// A raw object must be valid.
    pub const unsafe fn from_raw(handle: MlirDialectHandle) -> Self {
        Self { raw: handle }
    }

    /// Converts a dialect handle into a raw object.
    pub const fn to_raw(self) -> MlirDialectHandle {
        self.raw
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn func() {
        DialectHandle::func();
    }

    #[test]
    fn llvm() {
        DialectHandle::llvm();
    }

    #[test]
    fn namespace() {
        DialectHandle::func().namespace();
    }

    #[test]
    fn insert_dialect() {
        let registry = DialectRegistry::new();

        DialectHandle::func().insert_dialect(&registry);
    }

    #[test]
    fn load_dialect() {
        let context = Context::new();

        DialectHandle::func().load_dialect(&context);
    }

    #[test]
    fn register_dialect() {
        let context = Context::new();

        DialectHandle::func().register_dialect(&context);
    }
}
