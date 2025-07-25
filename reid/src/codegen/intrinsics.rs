use std::marker::PhantomData;

use reid_lib::{builder::InstructionValue, Instr};

use crate::{
    codegen::{ErrorKind, StackValueKind},
    mir::{BinaryOperator, BinopDefinition, FunctionDefinition, FunctionDefinitionKind, TypeKind},
};

use super::scope::{Scope, StackValue};

pub fn form_intrinsics() -> Vec<FunctionDefinition> {
    let intrinsics = Vec::new();

    intrinsics
}

pub fn form_intrinsic_binops() -> Vec<BinopDefinition> {
    let mut intrinsics = Vec::new();

    intrinsics.push(BinopDefinition {
        lhs: ("lhs".to_owned(), TypeKind::U32),
        op: BinaryOperator::Add,
        rhs: ("rhs".to_owned(), TypeKind::U32),
        return_type: TypeKind::U32,
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleInstr(
            |scope, lhs, rhs| scope.block.build(Instr::Add(lhs, rhs)).unwrap(),
        ))),
        meta: Default::default(),
    });

    intrinsics.push(BinopDefinition {
        lhs: ("lhs".to_owned(), TypeKind::U16),
        op: BinaryOperator::Add,
        rhs: ("rhs".to_owned(), TypeKind::U16),
        return_type: TypeKind::U16,
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleInstr(
            |scope, lhs, rhs| scope.block.build(Instr::Add(lhs, rhs)).unwrap(),
        ))),
        meta: Default::default(),
    });

    intrinsics.push(BinopDefinition {
        lhs: ("lhs".to_owned(), TypeKind::U32),
        op: BinaryOperator::Mult,
        rhs: ("rhs".to_owned(), TypeKind::U32),
        return_type: TypeKind::U32,
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleInstr(
            |scope, lhs, rhs| scope.block.build(Instr::Mul(lhs, rhs)).unwrap(),
        ))),
        meta: Default::default(),
    });

    intrinsics.push(BinopDefinition {
        lhs: ("lhs".to_owned(), TypeKind::U16),
        op: BinaryOperator::Mult,
        rhs: ("rhs".to_owned(), TypeKind::U16),
        return_type: TypeKind::U16,
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleInstr(
            |scope, lhs, rhs| scope.block.build(Instr::Mul(lhs, rhs)).unwrap(),
        ))),
        meta: Default::default(),
    });

    intrinsics.push(BinopDefinition {
        lhs: ("lhs".to_owned(), TypeKind::U32),
        op: BinaryOperator::Minus,
        rhs: ("rhs".to_owned(), TypeKind::U32),
        return_type: TypeKind::U32,
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleInstr(
            |scope, lhs, rhs| scope.block.build(Instr::Sub(lhs, rhs)).unwrap(),
        ))),
        meta: Default::default(),
    });

    intrinsics.push(BinopDefinition {
        lhs: ("lhs".to_owned(), TypeKind::U16),
        op: BinaryOperator::Minus,
        rhs: ("rhs".to_owned(), TypeKind::U16),
        return_type: TypeKind::U16,
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleInstr(
            |scope, lhs, rhs| scope.block.build(Instr::Sub(lhs, rhs)).unwrap(),
        ))),
        meta: Default::default(),
    });

    intrinsics
}

pub trait IntrinsicFunction: std::fmt::Debug {
    fn codegen<'ctx, 'a>(
        &self,
        scope: &mut Scope<'ctx, 'a>,
        params: &[&StackValue],
    ) -> Result<StackValue, ErrorKind>;
}

#[derive(Clone)]
pub struct IntrinsicSimpleInstr<T>(T)
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue;

impl<T> std::fmt::Debug for IntrinsicSimpleInstr<T>
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("IntrinsicSimpleInstr").finish()
    }
}

impl<T: Clone> IntrinsicFunction for IntrinsicSimpleInstr<T>
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue,
{
    fn codegen<'b, 'c>(
        &self,
        scope: &mut Scope<'b, 'c>,
        params: &[&StackValue],
    ) -> Result<StackValue, ErrorKind> {
        let lhs = params.get(0).unwrap();
        let rhs = params.get(1).unwrap();
        let instr = self.clone().0(scope, lhs.instr(), rhs.instr());
        Ok(StackValue(StackValueKind::Literal(instr), lhs.1.clone()))
    }
}

// impl IntrinsicFunction for IntrinsicIAdd {
//     fn codegen<'ctx, 'a>(
//         &self,
//         scope: &mut Scope<'ctx, 'a>,
//         params: &[InstructionValue],
//     ) -> Result<StackValue, ErrorKind> {
//         let lhs = params.get(0).unwrap();
//         let rhs = params.get(1).unwrap();
//         let add = scope.block.build(Instr::Add(*lhs, *rhs)).unwrap();
//         Ok(StackValue(StackValueKind::Literal(add), self.0.clone()))
//     }
// }

// #[derive(Debug, Clone)]
// pub struct IntrinsicIAdd(TypeKind);

// impl IntrinsicFunction for IntrinsicIAdd {
//     fn codegen<'ctx, 'a>(
//         &self,
//         scope: &mut Scope<'ctx, 'a>,
//         params: &[InstructionValue],
//     ) -> Result<StackValue, ErrorKind> {
//         let lhs = params.get(0).unwrap();
//         let rhs = params.get(1).unwrap();
//         let add = scope.block.build(Instr::Add(*lhs, *rhs)).unwrap();
//         Ok(StackValue(StackValueKind::Literal(add), self.0.clone()))
//     }
// }

// #[derive(Debug, Clone)]
// pub struct IntrinsicUDiv(TypeKind);

// impl IntrinsicFunction for IntrinsicUDiv {
//     fn codegen<'ctx, 'a>(
//         &self,
//         scope: &mut Scope<'ctx, 'a>,
//         params: &[InstructionValue],
//     ) -> Result<StackValue, ErrorKind> {
//         let lhs = params.get(0).unwrap();
//         let rhs = params.get(1).unwrap();
//         let add = scope.block.build(Instr::UDiv(*lhs, *rhs)).unwrap();
//         Ok(StackValue(StackValueKind::Literal(add), self.0.clone()))
//     }
// }

// #[derive(Debug, Clone)]
// pub struct IntrinsicUMod(TypeKind);

// impl IntrinsicFunction for IntrinsicUMod {
//     fn codegen<'ctx, 'a>(
//         &self,
//         scope: &mut Scope<'ctx, 'a>,
//         params: &[InstructionValue],
//     ) -> Result<StackValue, ErrorKind> {
//         let lhs = params.get(0).unwrap();
//         let rhs = params.get(1).unwrap();
//         let div = scope.block.build(Instr::UDiv(*lhs, *rhs)).unwrap();
//         let mul = scope.block.build(Instr::Mul(*rhs, div)).unwrap();
//         let sub = scope.block.build(Instr::Sub(*lhs, mul)).unwrap();
//         Ok(StackValue(StackValueKind::Literal(sub), self.0.clone()))
//     }
// }
