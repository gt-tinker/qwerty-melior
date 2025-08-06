use super::{Attribute, AttributeLike};
use crate::{
    ir::{Type, TypeLike},
    utility::ubig_to_llvm_apint_bigvals,
    Context, Error,
};
use dashu::integer::UBig;
use qwerty_mlir_sys::{
    mlirIntegerAttrBigIntGet, mlirIntegerAttrGet, mlirIntegerAttrGetValueInt,
    mlirIntegerAttrGetValueSInt, mlirIntegerAttrGetValueUInt, MlirAttribute,
};

/// An integer attribute.
#[derive(Clone, Copy)]
pub struct IntegerAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> IntegerAttribute<'c> {
    /// Creates an integer attribute.
    pub fn new(r#type: Type<'c>, integer: i64) -> Self {
        unsafe { Self::from_raw(mlirIntegerAttrGet(r#type.to_raw(), integer)) }
    }

    /// Creates an integer attribute from a bigint.
    pub fn from_ubig(context: &'c Context, bit_width: u64, val: &UBig) -> Self {
        let val_chunks = ubig_to_llvm_apint_bigvals(val);
        unsafe {
            Self::from_raw(mlirIntegerAttrBigIntGet(
                context.to_raw(),
                bit_width,
                val_chunks.len() as isize,
                val_chunks.as_ptr() as *const _ as *const _,
            ))
        }
    }

    /// Returns a value.
    pub fn value(&self) -> i64 {
        unsafe { mlirIntegerAttrGetValueInt(self.to_raw()) }
    }

    /// Returns a value as a signed integer.
    pub fn signed_value(&self) -> i64 {
        unsafe { mlirIntegerAttrGetValueSInt(self.to_raw()) }
    }

    /// Returns a value as an unsigned integer.
    pub fn unsigned_value(&self) -> u64 {
        unsafe { mlirIntegerAttrGetValueUInt(self.to_raw()) }
    }
}

attribute_traits!(IntegerAttribute, is_integer, "integer");

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ir::r#type::IntegerType, test::create_test_context};

    #[test]
    fn value() {
        let context = create_test_context();

        assert_eq!(
            IntegerAttribute::new(IntegerType::new(&context, 64).into(), 42).value(),
            42
        );
    }
}
