//! `qwerty` dialect.

use crate::{
    ir::{
        attribute::{StringAttribute, TypeAttribute},
        operation::OperationBuilder,
        r#type::{self, TypeLike},
        Attribute, Identifier, Location, Operation, Region, Type, Value,
    },
    Context,
};
use qwerty_mlir_sys::{mlirQwertyBitBundleTypeGet, mlirQwertyFunctionTypeGet, MlirType};
use std::fmt;

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
}

impl<'c> TypeLike<'c> for FunctionType<'c> {
    fn to_raw(&self) -> MlirType {
        self.r#type.to_raw()
    }
}

impl<'c> fmt::Display for FunctionType<'c> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.r#type, formatter)
    }
}

/// qwerty::BitBundleType. Corresponds to a register of bits, bit[n].
#[derive(Clone, Copy, Debug)]
pub struct BitBundleType<'c> {
    r#type: Type<'c>,
}

impl<'c> BitBundleType<'c> {
    /// Creates a bit bundle type.
    pub fn new(context: &'c Context, dim: usize) -> Self {
        Self {
            r#type: unsafe {
                Type::from_raw(mlirQwertyBitBundleTypeGet(context.to_raw(), dim as u64))
            },
        }
    }
}

impl<'c> TypeLike<'c> for BitBundleType<'c> {
    fn to_raw(&self) -> MlirType {
        self.r#type.to_raw()
    }
}

impl<'c> fmt::Display for BitBundleType<'c> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.r#type, formatter)
    }
}

from_subtypes!(Type, FunctionType, BitBundleType);

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
