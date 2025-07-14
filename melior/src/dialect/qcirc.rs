//! `qcirc` dialect.

use crate::ir::{operation::OperationBuilder, Location, Operation, Region, Type, Value};

/// Create a `qcirc.calc` operation.
pub fn calc<'c>(
    region: Region<'c>,
    result_types: &[Type<'c>],
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qcirc.calc", location)
        .add_regions([region])
        .add_results(result_types)
        .build()
        .expect("valid operation")
}

/// Create a `qcirc.calc_yield` operation.
pub fn calc_yield<'c>(operands: &[Value<'c, '_>], location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qcirc.calc_yield", location)
        .add_operands(operands)
        .build()
        .expect("valid operation")
}
