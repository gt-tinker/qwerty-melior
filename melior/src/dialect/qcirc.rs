//! `qcirc` dialect.

use crate::{
    ir::{operation::OperationBuilder, Location, Operation, OperationRef, Region, Type, Value},
    logical_result::LogicalResult,
};
use qwerty_mlir_sys::{mlirQCircGenerateQasm, mlirQCircGenerateQasmDestroyBuf};
use std::ffi::CStr;
use std::os::raw::c_char;

// Ops

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

// Passes

melior_macro::passes!(
    "QCirc",
    [
        mlirCreateQCircDecomposeMultiControl,
        mlirCreateQCircReplaceUnusualGates,
        mlirCreateQCircBarencoDecompose,
        mlirCreateQCircPeepholeOptimization,
        mlirCreateQCircInlineAdj,
        mlirCreateQCircBaseProfileModulePrep,
        mlirCreateQCircBaseProfileFuncPrep,
        mlirCreateQCircQCircToQIRConversion,
    ]
);

// Utilities

pub fn generate_qasm<'c>(func_op: OperationRef<'_, 'c>, print_locs: bool) -> Option<String> {
    unsafe {
        let mut buf_ptr: *mut c_char = std::ptr::null_mut();
        let res = LogicalResult::from_raw(mlirQCircGenerateQasm(
            func_op.to_raw(),
            print_locs,
            &mut buf_ptr,
        ));

        let ret = if res.is_failure() {
            None
        } else {
            Some(
                CStr::from_ptr(buf_ptr)
                    .to_str()
                    .expect("QASM is invalid UTF-8")
                    .to_string(),
            )
        };

        mlirQCircGenerateQasmDestroyBuf(buf_ptr);

        ret
    }
}
