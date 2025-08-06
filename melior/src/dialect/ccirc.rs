//! `ccirc` dialect.

use crate::{
    ir::{
        attribute::{BoolAttribute, IntegerAttribute, StringAttribute},
        operation::OperationBuilder,
        r#type::TypeLike,
        symbol_table::Visibility,
        Identifier, Location, Operation, Region, Type, Value,
    },
    type_traits, Context, Error,
};
use qwerty_mlir_sys::{mlirCCircWireTypeGet, MlirType};

// Types

/// ccirc::WireType. Corresponds to an n-bit wire bundle, `wire[n]`.
#[derive(Clone, Copy, Debug)]
pub struct WireType<'c> {
    r#type: Type<'c>,
}

impl<'c> WireType<'c> {
    /// Creates a wire type.
    pub fn new(context: &'c Context, dim: u64) -> Self {
        Self {
            r#type: unsafe { Type::from_raw(mlirCCircWireTypeGet(context.to_raw(), dim)) },
        }
    }
}

type_traits!(WireType, is_c_circ_wire, "ccirc wire");

from_subtypes!(Type, WireType);

// Ops

/// Create a `ccirc.circuit` operation.
pub fn circuit<'c>(
    context: &'c Context,
    reversible: BoolAttribute<'c>,
    name: StringAttribute<'c>,
    sym_visibility: Visibility,
    region: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("ccirc.circuit", location)
        .add_attributes(&[
            (Identifier::new(context, "reversible"), reversible.into()),
            (Identifier::new(context, "sym_name"), name.into()),
        ])
        .add_attributes(&sym_visibility.to_attribute_list(context))
        .add_regions([region])
        .build()
        .expect("valid operation")
}

/// Create a `ccirc.return` operation.
pub fn r#return<'c>(operands: &[Value<'c, '_>], location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("ccirc.return", location)
        .add_operands(operands)
        .build()
        .expect("valid operation")
}

/// Create a `ccirc.constant` operation.
pub fn constant<'c>(
    context: &'c Context,
    value: IntegerAttribute<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("ccirc.constant", location)
        .add_attributes(&[(Identifier::new(context, "value"), value.into())])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `ccirc.and` operation.
pub fn and<'c>(left: Value<'c, '_>, right: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("ccirc.and", location)
        .add_operands(&[left, right])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `ccirc.or` operation.
pub fn or<'c>(left: Value<'c, '_>, right: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("ccirc.or", location)
        .add_operands(&[left, right])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `ccirc.xor` operation.
pub fn xor<'c>(left: Value<'c, '_>, right: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("ccirc.xor", location)
        .add_operands(&[left, right])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `ccirc.not` operation.
pub fn not<'c>(operand: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("ccirc.not", location)
        .add_operands(&[operand])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `ccirc.wirepack` operation.
pub fn wirepack<'c>(wires: &[Value<'c, '_>], location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("ccirc.wirepack", location)
        .add_operands(wires)
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `ccirc.wireunpack` operation.
pub fn wireunpack<'c>(wire: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("ccirc.wireunpack", location)
        .add_operands(&[wire])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}
