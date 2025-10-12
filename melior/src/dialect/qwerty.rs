//! `qwerty` dialect.

use crate::{
    attribute_traits, attribute_traits_no_try_from,
    ir::{
        attribute::AttributeLike,
        attribute::{
            FlatSymbolRefAttribute, FloatAttribute, IntegerAttribute, StringAttribute,
            TypeAttribute,
        },
        operation::OperationBuilder,
        r#type::{self, IntegerType, TypeLike},
        symbol_table::Visibility,
        Attribute, Identifier, Location, Operation, Region, Type, Value,
    },
    type_traits,
    utility::ubig_to_llvm_apint_bigvals,
    Context, Error,
};
use dashu::integer::UBig;
use qwerty_mlir_sys::{
    mlirQwertyApplyRevolveGeneratorAttrGet, mlirQwertyBasisAttrGet, mlirQwertyBasisAttrGetDim,
    mlirQwertyBasisElemAttrGetFromRevolve, mlirQwertyBasisElemAttrGetFromStd,
    mlirQwertyBasisElemAttrGetFromVeclist, mlirQwertyBasisVectorAttrGet,
    mlirQwertyBasisVectorAttrGetHasPhase, mlirQwertyBasisVectorListAttrGet,
    mlirQwertyBitBundleTypeGet, mlirQwertyBuiltinBasisAttrGet, mlirQwertyFunctionTypeGet,
    mlirQwertyFunctionTypeGetFunctionType, mlirQwertyQBundleTypeGet, mlirQwertySuperposAttrGet,
    mlirQwertySuperposElemAttrGet, MlirAttribute, MlirType,
};

// Enums

/// Corresponds to qwerty::EigenstateAttr.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Eigenstate {
    Plus,
    Minus,
}

/// Corresponds to qwerty::PrimitiveBasisAttr.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveBasis {
    X,
    Y,
    Z,
    Fourier,
    Bell,
}

// Types

/// qwerty::FunctionType. This is an mlir::FunctionType along with a reversible bit.
#[derive(Clone, Copy, Debug)]
pub struct FunctionType<'c> {
    r#type: Type<'c>,
}

impl<'c> FunctionType<'c> {
    /// Creates a function type.
    pub fn new(
        context: &'c Context,
        func_type: r#type::FunctionType<'c>,
        reversible: bool,
    ) -> Self {
        Self {
            r#type: unsafe {
                Type::from_raw(mlirQwertyFunctionTypeGet(
                    context.to_raw(),
                    func_type.to_raw(),
                    reversible,
                ))
            },
        }
    }

    pub fn get_function_type(&self) -> r#type::FunctionType<'c> {
        unsafe {
            r#type::FunctionType::from_raw(mlirQwertyFunctionTypeGetFunctionType(
                self.r#type.to_raw(),
            ))
        }
    }
}

type_traits!(FunctionType, is_qwerty_function, "qwerty function");

/// qwerty::BitBundleType. Corresponds to a register of bits, `bit[n]`.
#[derive(Clone, Copy, Debug)]
pub struct BitBundleType<'c> {
    r#type: Type<'c>,
}

impl<'c> BitBundleType<'c> {
    /// Creates a bit bundle type.
    pub fn new(context: &'c Context, dim: u64) -> Self {
        Self {
            r#type: unsafe { Type::from_raw(mlirQwertyBitBundleTypeGet(context.to_raw(), dim)) },
        }
    }
}

type_traits!(BitBundleType, is_qwerty_bit_bundle, "qwerty bit bundle");

/// qwerty::QBundleType. Corresponds to a register of qubits, `qbundle[n]`.
#[derive(Clone, Copy, Debug)]
pub struct QBundleType<'c> {
    r#type: Type<'c>,
}

impl<'c> QBundleType<'c> {
    /// Creates a bit bundle type.
    pub fn new(context: &'c Context, dim: u64) -> Self {
        Self {
            r#type: unsafe { Type::from_raw(mlirQwertyQBundleTypeGet(context.to_raw(), dim)) },
        }
    }
}

type_traits!(QBundleType, is_qwerty_q_bundle, "qwerty qubit bundle");

from_subtypes!(Type, FunctionType, BitBundleType, QBundleType);

// Attributes

/// qwerty::SuperposAttr
#[derive(Clone, Copy)]
pub struct SuperposAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> SuperposAttribute<'c> {
    /// Creates a qwerty::SuperposAttr.
    pub fn new(context: &'c Context, values: &[SuperposElemAttribute<'c>]) -> Self {
        unsafe {
            Self::from_raw(mlirQwertySuperposAttrGet(
                context.to_raw(),
                values.len() as isize,
                values.as_ptr() as *const _ as *const _,
            ))
        }
    }
}

attribute_traits!(SuperposAttribute, is_qwerty_superpos, "qwerty superpos");

/// qwerty::SuperposElemAttr
#[derive(Clone, Copy)]
pub struct SuperposElemAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> SuperposElemAttribute<'c> {
    /// Creates a qwerty::SuperposElemAttr.
    pub fn new(
        context: &'c Context,
        prob: FloatAttribute<'c>,
        phase: FloatAttribute<'c>,
        vectors: &[BasisVectorAttribute<'c>],
    ) -> Self {
        unsafe {
            Self::from_raw(mlirQwertySuperposElemAttrGet(
                context.to_raw(),
                prob.to_raw(),
                phase.to_raw(),
                vectors.len() as isize,
                vectors.as_ptr() as *const _ as *const _,
            ))
        }
    }
}

attribute_traits!(
    SuperposElemAttribute,
    is_qwerty_superpos_elem,
    "qwerty superpos element"
);

/// qwerty::BuiltinBasisAttr
#[derive(Clone, Copy)]
pub struct BuiltinBasisAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> BuiltinBasisAttribute<'c> {
    /// Creates a qwerty::BuiltinBasisAttr.
    pub fn new(context: &'c Context, prim_basis: PrimitiveBasis, dim: u64) -> Self {
        unsafe {
            Self::from_raw(mlirQwertyBuiltinBasisAttrGet(
                context.to_raw(),
                prim_basis as i64,
                dim,
            ))
        }
    }
}

attribute_traits!(
    BuiltinBasisAttribute,
    is_qwerty_builtin_basis,
    "qwerty built-in basis"
);

/// qwerty::ApplyRevolveGeneratorAttr
#[derive(Clone, Copy)]
pub struct ApplyRevolveGeneratorAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> ApplyRevolveGeneratorAttribute<'c> {
    /// Creates a qwerty::ApplyRevolveGeneratorAttribute
    /// of the form foo // {bv1, bv2}.revolve
    pub fn new(
        context: &'c Context,
        // basisattr, two basisvectorattr
        foo: BasisAttribute<'c>,
        bv1: BasisVectorAttribute<'c>,
        bv2: BasisVectorAttribute<'c>,
    ) -> Self {
        unsafe {
            Self::from_raw(mlirQwertyApplyRevolveGeneratorAttrGet(
                context.to_raw(),
                foo.to_raw(),
                bv1.to_raw(),
                bv2.to_raw(),
            ))
        }
    }
}

attribute_traits!(
    ApplyRevolveGeneratorAttribute,
    is_qwerty_apply_revolve_generator,
    "qwerty apply revolve generator"
);

/// qwerty::BasisVectorAttr
#[derive(Clone, Copy)]
pub struct BasisVectorAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> BasisVectorAttribute<'c> {
    /// Creates a qwerty::BasisVectorAttr.
    pub fn new(
        context: &'c Context,
        prim_basis: PrimitiveBasis,
        eigenbits: UBig,
        dim: u64,
        has_phase: bool,
    ) -> Self {
        let chunks = ubig_to_llvm_apint_bigvals(&eigenbits);
        unsafe {
            Self::from_raw(mlirQwertyBasisVectorAttrGet(
                context.to_raw(),
                prim_basis as i64,
                dim,
                has_phase,
                chunks.len() as isize,
                chunks.as_ptr() as *const _ as *const _,
            ))
        }
    }

    /// Returns true iff this vector has a corresponding phase
    pub fn has_phase(&self) -> bool {
        unsafe { mlirQwertyBasisVectorAttrGetHasPhase(self.to_raw()) }
    }
}

attribute_traits!(
    BasisVectorAttribute,
    is_qwerty_basis_vector,
    "qwerty basis vector"
);

/// qwerty::BasisVectorListAttr
#[derive(Clone, Copy)]
pub struct BasisVectorListAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> BasisVectorListAttribute<'c> {
    /// Creates a qwerty::BasisVectorListAttr.
    pub fn new(context: &'c Context, vectors: &[BasisVectorAttribute<'c>]) -> Self {
        unsafe {
            Self::from_raw(mlirQwertyBasisVectorListAttrGet(
                context.to_raw(),
                vectors.len() as isize,
                vectors.as_ptr() as *const _ as *const _,
            ))
        }
    }
}

attribute_traits!(
    BasisVectorListAttribute,
    is_qwerty_basis_vector_list,
    "qwerty basis vector list"
);

/// qwerty::BasisElemAttr
#[derive(Clone, Copy)]
pub struct BasisElemAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> BasisElemAttribute<'c> {
    /// Creates a qwerty::BasisElemAttr from a qwerty::BasisVectorListAttr.
    pub fn from_veclist(context: &'c Context, veclist: BasisVectorListAttribute<'c>) -> Self {
        unsafe {
            Self::from_raw(mlirQwertyBasisElemAttrGetFromVeclist(
                context.to_raw(),
                veclist.to_raw(),
            ))
        }
    }

    /// Creates a qwerty::BasisElemAttr from a qwerty::BuiltinBasis.
    pub fn from_std(context: &'c Context, std: BuiltinBasisAttribute<'c>) -> Self {
        unsafe {
            Self::from_raw(mlirQwertyBasisElemAttrGetFromStd(
                context.to_raw(),
                std.to_raw(),
            ))
        }
    }

    pub fn from_revolve(context: &'c Context, revolve: ApplyRevolveGeneratorAttribute<'c>) -> Self {
        unsafe {
            Self::from_raw(mlirQwertyBasisElemAttrGetFromRevolve(
                context.to_raw(),
                revolve.to_raw(),
            ))
        }
    }
}

attribute_traits!(
    BasisElemAttribute,
    is_qwerty_basis_elem,
    "qwerty basis element"
);

/// qwerty::BasisAttr
#[derive(Clone, Copy)]
pub struct BasisAttribute<'c> {
    attribute: Attribute<'c>,
}

impl<'c> BasisAttribute<'c> {
    /// Creates a qwerty::BasisAttr.
    pub fn new(context: &'c Context, elems: &[BasisElemAttribute<'c>]) -> Self {
        unsafe {
            Self::from_raw(mlirQwertyBasisAttrGet(
                context.to_raw(),
                elems.len() as isize,
                elems.as_ptr() as *const _ as *const _,
            ))
        }
    }

    /// Returns the dimension of this basis.
    pub fn get_dim(&self) -> u64 {
        unsafe { mlirQwertyBasisAttrGetDim(self.to_raw()) }
    }
}

attribute_traits!(BasisAttribute, is_qwerty_basis_vector_list, "qwerty basis");

from_subtypes!(
    Attribute,
    SuperposAttribute,
    SuperposElemAttribute,
    ApplyRevolveGeneratorAttribute,
    BasisVectorAttribute,
    BasisVectorListAttribute,
    BasisElemAttribute,
    BasisAttribute,
);

// Ops

/// Create a `qwerty.func` operation.
pub fn func<'c>(
    context: &'c Context,
    name: StringAttribute<'c>,
    func_ty: TypeAttribute<'c>,
    sym_visibility: Visibility,
    region: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.func", location)
        .add_attributes(&[
            (Identifier::new(context, "sym_name"), name.into()),
            (Identifier::new(context, "qwerty_func_type"), func_ty.into()),
        ])
        .add_attributes(&sym_visibility.to_attribute_list(context))
        .add_regions([region])
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.return` operation.
pub fn r#return<'c>(operands: &[Value<'c, '_>], location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qwerty.return", location)
        .add_operands(operands)
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.func_const` operation.
pub fn func_const<'c>(
    context: &'c Context,
    func: FlatSymbolRefAttribute<'c>,
    captures: &[Value<'c, '_>],
    r#type: FunctionType<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.func_const", location)
        .add_attributes(&[(Identifier::new(context, "func"), func.into())])
        .add_operands(captures)
        .add_results(&[r#type.into()])
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.func_adj` operation.
pub fn func_adj<'c>(callee: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qwerty.func_adj", location)
        .add_operands(&[callee])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.func_pred` operation.
pub fn func_pred<'c>(
    context: &'c Context,
    pred: BasisAttribute<'c>,
    callee: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.func_pred", location)
        .add_attributes(&[(Identifier::new(context, "pred"), pred.into())])
        .add_operands(&[callee])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.call_indirect` operation.
pub fn call_indirect<'c>(
    callee: Value<'c, '_>,
    operands: &[Value<'c, '_>],
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.call_indirect", location)
        .add_operands(&[callee])
        .add_operands(operands)
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.lambda` operation.
pub fn lambda<'c>(
    captures: &[Value<'c, '_>],
    ty: FunctionType<'c>,
    region: Region<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.lambda", location)
        .add_operands(captures)
        .add_results(&[ty.into()])
        .add_regions([region])
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.bitpack` operation.
pub fn bitpack<'c>(bits: &[Value<'c, '_>], location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qwerty.bitpack", location)
        .add_operands(bits)
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.bitunpack` operation.
pub fn bitunpack<'c>(bundle: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qwerty.bitunpack", location)
        .add_operands(&[bundle])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.qbpack` operation.
pub fn qbpack<'c>(qubits: &[Value<'c, '_>], location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qwerty.qbpack", location)
        .add_operands(qubits)
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.qbunpack` operation.
pub fn qbunpack<'c>(qbundle: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qwerty.qbunpack", location)
        .add_operands(&[qbundle])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.qbprep` operation.
pub fn qbprep<'c>(
    context: &'c Context,
    prim_basis: PrimitiveBasis,
    eigenstate: Eigenstate,
    dim: IntegerAttribute<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.qbprep", location)
        .add_attributes(&[
            (
                Identifier::new(context, "prim_basis"),
                IntegerAttribute::new(IntegerType::new(context, 64).into(), prim_basis as i64)
                    .into(),
            ),
            (
                Identifier::new(context, "eigenstate"),
                IntegerAttribute::new(IntegerType::new(context, 64).into(), eigenstate as i64)
                    .into(),
            ),
            (Identifier::new(context, "dim"), dim.into()),
        ])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.qbdiscard` operation.
pub fn qbdiscard<'c>(qbundle: Value<'c, '_>, location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qwerty.qbdiscard", location)
        .add_operands(&[qbundle])
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.qbtrans` operation.
pub fn qbtrans<'c>(
    context: &'c Context,
    basis_in: BasisAttribute<'c>,
    basis_out: BasisAttribute<'c>,
    phases: &[Value<'c, '_>],
    qbundle: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.qbtrans", location)
        .add_attributes(&[
            (Identifier::new(context, "basisIn"), basis_in.into()),
            (Identifier::new(context, "basisOut"), basis_out.into()),
        ])
        .add_operands(phases)
        .add_operands(&[qbundle])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.qbmeas` operation.
pub fn qbmeas<'c>(
    context: &'c Context,
    basis: BasisAttribute<'c>,
    qbundle: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.qbmeas", location)
        .add_attributes(&[(Identifier::new(context, "basis"), basis.into())])
        .add_operands(&[qbundle])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.qbphase` operation.
pub fn qbphase<'c>(
    theta: Value<'c, '_>,
    qbundle_in: Value<'c, '_>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.qbphase", location)
        .add_operands(&[theta, qbundle_in])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.superpos` operation.
pub fn superpos<'c>(
    context: &'c Context,
    sup: SuperposAttribute<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.superpos", location)
        .add_attributes(&[(Identifier::new(context, "superpos"), sup.into())])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.ensemble` operation.
pub fn ensemble<'c>(
    context: &'c Context,
    sup: SuperposAttribute<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.ensemble", location)
        .add_attributes(&[(Identifier::new(context, "superpos"), sup.into())])
        .enable_result_type_inference()
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.embed_xor` operation.
pub fn embed_xor<'c>(
    context: &'c Context,
    circuit: FlatSymbolRefAttribute<'c>,
    r#type: FunctionType<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.embed_xor", location)
        .add_attributes(&[(Identifier::new(context, "circuit"), circuit.into())])
        .add_results(&[r#type.into()])
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.embed_sign` operation.
pub fn embed_sign<'c>(
    context: &'c Context,
    circuit: FlatSymbolRefAttribute<'c>,
    r#type: FunctionType<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.embed_sign", location)
        .add_attributes(&[(Identifier::new(context, "circuit"), circuit.into())])
        .add_results(&[r#type.into()])
        .build()
        .expect("valid operation")
}

/// Create a `qwerty.embed_inplace` operation.
pub fn embed_inplace<'c>(
    context: &'c Context,
    circuit: FlatSymbolRefAttribute<'c>,
    r#type: FunctionType<'c>,
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.embed_inplace", location)
        .add_attributes(&[(Identifier::new(context, "circuit"), circuit.into())])
        .add_results(&[r#type.into()])
        .build()
        .expect("valid operation")
}

// Passes

melior_macro::passes!(
    "Qwerty",
    [
        mlirCreateQwertyInlinePred,
        mlirCreateQwertySynthEmbeds,
        mlirCreateQwertyLiftLambdas,
        mlirCreateQwertyOnlyPredOnes,
        mlirCreateQwertyQwertyToQCircConversion,
    ]
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ir::{r#type::IntegerType, Block, BlockLike, Module, OperationLike, RegionLike},
        test::create_test_context,
    };

    fn compile_operation<'c>(
        context: &'c Context,
        operation: impl Fn(&Block<'c>) -> Operation<'c>,
        function_type: r#type::FunctionType<'c>,
    ) {
        let location = Location::unknown(context);
        let module = Module::new(location);

        let block = Block::new(
            &(0..function_type.input_count())
                .map(|index| (function_type.input(index).unwrap(), location))
                .collect::<Vec<_>>(),
        );

        let operation = operation(&block);
        let name = operation.name();
        let name = name.as_string_ref().as_str().unwrap();

        block.append_operation(r#return(
            &[block.append_operation(operation).result(0).unwrap().into()],
            location,
        ));

        let region = Region::new();
        region.append_block(block);

        let function = func(
            context,
            StringAttribute::new(context, "foo"),
            TypeAttribute::new(
                FunctionType::new(context, function_type, /*reversible=*/ false).into(),
            ),
            Visibility::Public,
            region,
            Location::unknown(context),
        );

        module.body().append_operation(function);

        assert!(module.as_operation().verify());
        insta::assert_snapshot!(name, module.as_operation());
    }

    #[test]
    fn compile_function() {
        let context = create_test_context();

        let location = Location::unknown(&context);
        let module = Module::new(location);

        let integer_type = Type::index(&context);

        let function = {
            let block = Block::new(&[(integer_type, location)]);

            block.append_operation(r#return(&[block.argument(0).unwrap().into()], location));

            let region = Region::new();
            region.append_block(block);

            func(
                &context,
                StringAttribute::new(&context, "foo"),
                TypeAttribute::new(
                    FunctionType::new(
                        &context,
                        r#type::FunctionType::new(&context, &[integer_type], &[integer_type]),
                        /*reversible=*/ false,
                    )
                    .into(),
                ),
                Visibility::Public,
                region,
                Location::unknown(&context),
            )
        };

        module.body().append_operation(function);

        assert!(module.as_operation().verify());
        insta::assert_snapshot!(module.as_operation());
    }

    #[test]
    fn compile_bitpack() {
        let context = create_test_context();
        let bit_type: Type = IntegerType::new(&context, 1).into();
        let bitbundle3_type: Type = BitBundleType::new(&context, 3).into();

        compile_operation(
            &context,
            |block| {
                bitpack(
                    &[
                        block.argument(0).unwrap().into(),
                        block.argument(1).unwrap().into(),
                        block.argument(2).unwrap().into(),
                    ],
                    Location::unknown(&context),
                )
            },
            r#type::FunctionType::new(
                &context,
                &[bit_type, bit_type, bit_type],
                &[bitbundle3_type],
            ),
        );
    }
}
