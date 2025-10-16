use crate::{
    ir::{attribute::StringAttribute, Attribute, Identifier, Operation, OperationRef},
    Context, StringRef,
};
use qwerty_mlir_sys::{
    mlirSymbolTableCreate, mlirSymbolTableDestroy, mlirSymbolTableGetVisibilityAttributeName,
    mlirSymbolTableLookup, MlirSymbolTable,
};
use std::marker::PhantomData;

/// An `mlir::SymbolTable`.
#[derive(Debug)]
pub struct SymbolTable<'c> {
    raw: MlirSymbolTable,
    _context: PhantomData<&'c Context>,
}

impl<'c> SymbolTable<'c> {
    pub fn new(op: OperationRef<'_, 'c>) -> Option<SymbolTable<'c>> {
        unsafe { Self::from_option_raw(mlirSymbolTableCreate(op.to_raw())) }
    }

    pub unsafe fn from_raw(raw: MlirSymbolTable) -> Self {
        Self {
            raw,
            _context: Default::default(),
        }
    }

    pub unsafe fn from_option_raw(raw: MlirSymbolTable) -> Option<Self> {
        if raw.ptr.is_null() {
            None
        } else {
            Some(Self::from_raw(raw))
        }
    }

    pub const fn to_raw(&self) -> MlirSymbolTable {
        self.raw
    }

    pub fn lookup(&self, name: &str) -> Option<Operation<'c>> {
        unsafe {
            let name_ref = StringRef::new(name);
            let op = mlirSymbolTableLookup(self.to_raw(), name_ref.to_raw());
            Operation::from_option_raw(op)
        }
    }
}

impl<'c> Drop for SymbolTable<'c> {
    fn drop(&mut self) {
        unsafe { mlirSymbolTableDestroy(self.raw) }
    }
}

/// Corresponds to `mlir::SymbolTable::Visibility`.
pub enum Visibility {
    Public,
    Private,
    Nested,
}

impl Visibility {
    pub fn get_attribute_name() -> &'static str {
        unsafe {
            let str_ref = StringRef::from_raw(mlirSymbolTableGetVisibilityAttributeName());
            let string = str_ref
                .as_str()
                .expect("SymbolTable::getVisibilityAttrName() returned a non-UTF-8 string");
            // Treating this as 'static is okay because that is the assumption
            // behind the API call itself.
            string
        }
    }

    /// Returns a list of attributes suitable for passing to
    /// `OperationBuilder::add_attributes`.
    pub fn to_attribute_list<'c>(
        self,
        context: &'c Context,
    ) -> Vec<(Identifier<'c>, Attribute<'c>)> {
        match self {
            Visibility::Public => vec![],
            Visibility::Private => vec![(
                Identifier::new(context, Self::get_attribute_name()),
                StringAttribute::new(context, "private").into(),
            )],
            Visibility::Nested => vec![(
                Identifier::new(context, Self::get_attribute_name()),
                StringAttribute::new(context, "nested").into(),
            )],
        }
    }
}
