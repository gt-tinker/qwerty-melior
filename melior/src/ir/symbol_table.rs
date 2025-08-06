use crate::{
    ir::{attribute::StringAttribute, Attribute, Identifier},
    Context, StringRef,
};
use qwerty_mlir_sys::mlirSymbolTableGetVisibilityAttributeName;

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
