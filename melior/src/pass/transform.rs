//! Transform passes.

use crate::pass::Pass;
use qwerty_mlir_sys::mlirCreateTransformsInlinerWithOptions;
use std::ffi::CString;

melior_macro::passes!(
    "Transforms",
    [
        // spell-checker: disable-next-line
        mlirCreateTransformsCSE,
        mlirCreateTransformsCanonicalizer,
        mlirCreateTransformsCompositeFixedPointPass,
        mlirCreateTransformsControlFlowSink,
        mlirCreateTransformsGenerateRuntimeVerification,
        mlirCreateTransformsInliner,
        mlirCreateTransformsLocationSnapshot,
        mlirCreateTransformsLoopInvariantCodeMotion,
        mlirCreateTransformsLoopInvariantSubsetHoisting,
        mlirCreateTransformsMem2Reg,
        mlirCreateTransformsPrintIRPass,
        mlirCreateTransformsPrintOpStats,
        mlirCreateTransformsRemoveDeadValues,
        mlirCreateTransformsSCCP,
        mlirCreateTransformsSROA,
        mlirCreateTransformsStripDebugInfo,
        mlirCreateTransformsSymbolDCE,
        mlirCreateTransformsSymbolPrivatize,
        mlirCreateTransformsTopologicalSort,
        mlirCreateTransformsViewOpGraph,
    ]
);

pub fn create_inliner_with_options(options: &str) -> Pass {
    unsafe {
        let cstr = CString::new(options).expect("options must not contain a null byte");
        let pass = mlirCreateTransformsInlinerWithOptions(cstr.into_raw());
        Pass::from_raw(pass)
    }
}
