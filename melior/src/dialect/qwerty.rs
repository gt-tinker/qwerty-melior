//! `qwerty` dialect.

use crate::{
    attribute_traits,
    ir::{
        attribute::AttributeLike,
        attribute::{FloatAttribute, IntegerAttribute, StringAttribute, TypeAttribute},
        operation::OperationBuilder,
        r#type::{self, IntegerType, TypeLike},
        Attribute, Identifier, Location, Operation, Region, Type, Value,
    },
    type_traits, Context, Error,
};
use dashu::integer::UBig;
use qwerty_mlir_sys::{
    mlirQwertyBasisVectorAttrGet, mlirQwertyBitBundleTypeGet, mlirQwertyFunctionTypeGet,
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

/// qwerty::BitBundleType. Corresponds to a register of bits, bit[n].
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

/// qwerty::QBundleType. Corresponds to a register of qubits, qbundle[n].
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
        let chunks = eigenbits.as_words();

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
}

attribute_traits!(
    BasisVectorAttribute,
    is_qwerty_basis_vector,
    "qwerty basis vector"
);

from_subtypes!(
    Attribute,
    SuperposAttribute,
    SuperposElemAttribute,
    BasisVectorAttribute
);

// Ops

/// Create a `qwerty.func` operation.
pub fn func<'c>(
    context: &'c Context,
    name: StringAttribute<'c>,
    func_ty: TypeAttribute<'c>,
    region: Region<'c>,
    attributes: &[(Identifier<'c>, Attribute<'c>)],
    location: Location<'c>,
) -> Operation<'c> {
    OperationBuilder::new("qwerty.func", location)
        .add_attributes(&[
            (Identifier::new(context, "sym_name"), name.into()),
            (Identifier::new(context, "qwerty_func_type"), func_ty.into()),
        ])
        .add_attributes(attributes)
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

/// Create a `qwerty.bitpack` operation.
pub fn bitpack<'c>(bits: &[Value<'c, '_>], location: Location<'c>) -> Operation<'c> {
    OperationBuilder::new("qwerty.bitpack", location)
        .add_operands(bits)
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
            region,
            &[],
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
                region,
                &[],
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
