use reid_lib::{builder::InstructionValue, CmpPredicate, ConstValue, Instr, Type};

use crate::{
    codegen::{ErrorKind, StackValueKind},
    mir::{
        BinaryOperator, BinopDefinition, CmpOperator, FunctionDefinition, FunctionDefinitionKind, FunctionParam,
        TypeKind,
    },
};

use super::scope::{Scope, StackValue};

const INTEGERS: [TypeKind; 10] = [
    TypeKind::U8,
    TypeKind::U16,
    TypeKind::U32,
    TypeKind::U64,
    TypeKind::U128,
    TypeKind::I8,
    TypeKind::I16,
    TypeKind::I32,
    TypeKind::I64,
    TypeKind::I128,
];

const FLOATS: [TypeKind; 7] = [
    TypeKind::F16,
    TypeKind::F32,
    TypeKind::F32B,
    TypeKind::F64,
    TypeKind::F80,
    TypeKind::F128,
    TypeKind::F128PPC,
];

const INTRINSIC_IDENT: &str = "reid.intrinsic";
const MALLOC_IDENT: &str = "malloc";

pub fn form_intrinsics() -> Vec<FunctionDefinition> {
    let mut intrinsics = Vec::new();

    intrinsics.push(FunctionDefinition {
        name: MALLOC_IDENT.to_owned(),
        linkage_name: Some("malloc".to_owned()),
        is_pub: false,
        is_imported: true,
        return_type: TypeKind::UserPtr(Box::new(TypeKind::U8)),
        parameters: vec![FunctionParam {
            name: "size".to_owned(),
            ty: TypeKind::U64,
            meta: Default::default(),
        }],
        kind: FunctionDefinitionKind::Extern(false),
        source: None,
    });

    intrinsics
}

pub fn get_intrinsic_assoc_func(ty: &TypeKind, name: &str) -> Option<FunctionDefinition> {
    match name {
        "sizeof" => Some(FunctionDefinition {
            name: "sizeof".to_owned(),
            linkage_name: None,
            is_pub: true,
            is_imported: false,
            return_type: TypeKind::U64,
            parameters: Vec::new(),
            kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSizeOf(ty.clone()))),
            source: None,
        }),
        "malloc" => Some(FunctionDefinition {
            name: "malloc".to_owned(),
            linkage_name: None,
            is_pub: true,
            is_imported: false,
            return_type: TypeKind::UserPtr(Box::new(ty.clone())),
            parameters: vec![FunctionParam {
                name: String::from("size"),
                ty: TypeKind::U64,
                meta: Default::default(),
            }],
            kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicMalloc(ty.clone()))),
            source: None,
        }),
        "null" => Some(FunctionDefinition {
            name: "null".to_owned(),
            linkage_name: None,
            is_pub: true,
            is_imported: false,
            return_type: TypeKind::UserPtr(Box::new(ty.clone())),
            parameters: Vec::new(),
            kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicNullPtr(ty.clone()))),
            source: None,
        }),
        _ => None,
    }
}

fn simple_binop_def<T: Clone + 'static>(op: BinaryOperator, ty: &TypeKind, fun: T) -> BinopDefinition
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue,
{
    BinopDefinition {
        lhs: FunctionParam {
            name: "lhs".to_owned(),
            ty: ty.clone(),
            meta: Default::default(),
        },
        op,
        rhs: FunctionParam {
            name: "rhs".to_owned(),
            ty: ty.clone(),
            meta: Default::default(),
        },
        return_type: ty.clone(),
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleInstr(fun))),
        meta: Default::default(),
        exported: false,
    }
}

fn complex_binop_def<T: Clone + 'static>(op: BinaryOperator, lhs: &TypeKind, rhs: &TypeKind, fun: T) -> BinopDefinition
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue,
{
    BinopDefinition {
        lhs: FunctionParam {
            name: "lhs".to_owned(),
            ty: lhs.clone(),
            meta: Default::default(),
        },
        op,
        rhs: FunctionParam {
            name: "rhs".to_owned(),
            ty: rhs.clone(),
            meta: Default::default(),
        },
        return_type: lhs.clone(),
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleInstr(fun))),
        meta: Default::default(),
        exported: false,
    }
}

fn boolean_binop_def<T: Clone + 'static>(op: BinaryOperator, ty: &TypeKind, fun: T) -> BinopDefinition
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue,
{
    BinopDefinition {
        lhs: FunctionParam {
            name: "lhs".to_owned(),
            ty: ty.clone(),
            meta: Default::default(),
        },
        op,
        rhs: FunctionParam {
            name: "rhs".to_owned(),
            ty: ty.clone(),
            meta: Default::default(),
        },
        return_type: TypeKind::Bool,
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicBooleanInstr(fun))),
        meta: Default::default(),
        exported: false,
    }
}

pub fn form_intrinsic_binops() -> Vec<BinopDefinition> {
    let mut intrinsics = Vec::new();

    use BinaryOperator::*;

    for ty in INTEGERS {
        intrinsics.push(simple_binop_def(Add, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::Add(lhs, rhs)).unwrap()
        }));
        intrinsics.push(simple_binop_def(Mult, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::Mul(lhs, rhs)).unwrap()
        }));
        intrinsics.push(simple_binop_def(Minus, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::Sub(lhs, rhs)).unwrap()
        }));
        if ty.signed() {
            intrinsics.push(simple_binop_def(Div, &ty, |scope, lhs, rhs| {
                scope.block.build(Instr::SDiv(lhs, rhs)).unwrap()
            }));
            intrinsics.push(simple_binop_def(Mod, &ty, |scope, lhs, rhs| {
                let div = scope.block.build(Instr::SDiv(lhs, rhs)).unwrap();
                let mul = scope.block.build(Instr::Mul(rhs, div)).unwrap();
                scope.block.build(Instr::Sub(lhs, mul)).unwrap()
            }));
        } else {
            intrinsics.push(simple_binop_def(Div, &ty, |scope, lhs, rhs| {
                scope.block.build(Instr::UDiv(lhs, rhs)).unwrap()
            }));
            intrinsics.push(simple_binop_def(Mod, &ty, |scope, lhs, rhs| {
                let div = scope.block.build(Instr::UDiv(lhs, rhs)).unwrap();
                let mul = scope.block.build(Instr::Mul(rhs, div)).unwrap();
                scope.block.build(Instr::Sub(lhs, mul)).unwrap()
            }));
        }
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::GT), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::ICmp(CmpPredicate::GT, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::GE), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::ICmp(CmpPredicate::GE, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::LT), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::ICmp(CmpPredicate::LT, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::LE), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::ICmp(CmpPredicate::LE, lhs, rhs)).unwrap()
        }));

        // Bitwise operations
        intrinsics.push(simple_binop_def(BitOr, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::Or(lhs, rhs)).unwrap()
        }));
        intrinsics.push(simple_binop_def(BitAnd, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::And(lhs, rhs)).unwrap()
        }));

        intrinsics.push(complex_binop_def(Xor, &ty, &TypeKind::U64, |scope, lhs, rhs| {
            scope.block.build(Instr::XOr(lhs, rhs)).unwrap()
        }));
        if ty.signed() {
            intrinsics.push(complex_binop_def(
                BitshiftRight,
                &ty,
                &TypeKind::U64,
                |scope, lhs, rhs| scope.block.build(Instr::ShiftRightArithmetic(lhs, rhs)).unwrap(),
            ));
        } else {
            intrinsics.push(complex_binop_def(
                BitshiftRight,
                &ty,
                &TypeKind::U64,
                |scope, lhs, rhs| scope.block.build(Instr::ShiftRightLogical(lhs, rhs)).unwrap(),
            ));
        }
        intrinsics.push(complex_binop_def(
            BitshiftLeft,
            &ty,
            &TypeKind::U64,
            |scope, lhs, rhs| scope.block.build(Instr::ShiftLeft(lhs, rhs)).unwrap(),
        ));
    }
    for ty in INTEGERS.iter().chain(&[TypeKind::Bool, TypeKind::Char]) {
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::EQ), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::ICmp(CmpPredicate::EQ, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::NE), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::ICmp(CmpPredicate::NE, lhs, rhs)).unwrap()
        }));
    }
    for ty in FLOATS {
        intrinsics.push(simple_binop_def(BinaryOperator::Add, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FAdd(lhs, rhs)).unwrap()
        }));
        intrinsics.push(simple_binop_def(BinaryOperator::Mult, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FMul(lhs, rhs)).unwrap()
        }));
        intrinsics.push(simple_binop_def(BinaryOperator::Minus, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FSub(lhs, rhs)).unwrap()
        }));
        intrinsics.push(simple_binop_def(Div, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FDiv(lhs, rhs)).unwrap()
        }));
        intrinsics.push(simple_binop_def(Mod, &ty, |scope, lhs, rhs| {
            let div = scope.block.build(Instr::FDiv(lhs, rhs)).unwrap();
            let mul = scope.block.build(Instr::Mul(rhs, div)).unwrap();
            scope.block.build(Instr::Sub(lhs, mul)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::EQ), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FCmp(CmpPredicate::EQ, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::NE), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FCmp(CmpPredicate::NE, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::GT), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FCmp(CmpPredicate::GT, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::GE), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FCmp(CmpPredicate::GE, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::LT), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FCmp(CmpPredicate::LT, lhs, rhs)).unwrap()
        }));
        intrinsics.push(boolean_binop_def(Cmp(CmpOperator::LE), &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::FCmp(CmpPredicate::LE, lhs, rhs)).unwrap()
        }));
    }

    intrinsics.push(boolean_binop_def(And, &TypeKind::Bool, |scope, lhs, rhs| {
        scope.block.build(Instr::And(lhs, rhs)).unwrap()
    }));
    intrinsics.push(boolean_binop_def(Or, &TypeKind::Bool, |scope, lhs, rhs| {
        scope.block.build(Instr::Or(lhs, rhs)).unwrap()
    }));
    intrinsics.push(boolean_binop_def(Xor, &TypeKind::Bool, |scope, lhs, rhs| {
        scope.block.build(Instr::XOr(lhs, rhs)).unwrap()
    }));

    intrinsics
}

pub trait IntrinsicFunction: std::fmt::Debug {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, params: &[StackValue]) -> Result<StackValue, ErrorKind>;
}

macro_rules! intrinsic_debug {
    ($kind:ty, $name:literal) => {
        impl<T> std::fmt::Debug for $kind
        where
            T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple($name).finish()
            }
        }
    };
}

#[derive(Clone)]
pub struct IntrinsicSimpleInstr<T>(T)
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue;
intrinsic_debug!(IntrinsicSimpleInstr<T>, "IntrinsicSimpleInstr");

impl<T: Clone> IntrinsicFunction for IntrinsicSimpleInstr<T>
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue,
{
    fn codegen<'b, 'c>(&self, scope: &mut Scope<'b, 'c>, params: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let lhs = params.get(0).unwrap();
        let rhs = params.get(1).unwrap();
        let instr = self.clone().0(scope, lhs.instr(), rhs.instr());
        Ok(StackValue(StackValueKind::Literal(instr), lhs.1.clone()))
    }
}

#[derive(Clone)]
pub struct IntrinsicBooleanInstr<T>(T)
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue;
intrinsic_debug!(IntrinsicBooleanInstr<T>, "IntrinsicBooleanInstr");

impl<T: Clone> IntrinsicFunction for IntrinsicBooleanInstr<T>
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue,
{
    fn codegen<'b, 'c>(&self, scope: &mut Scope<'b, 'c>, params: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let lhs = params.get(0).unwrap();
        let rhs = params.get(1).unwrap();
        let instr = self.clone().0(scope, lhs.instr(), rhs.instr());
        Ok(StackValue(StackValueKind::Literal(instr), TypeKind::Bool))
    }
}

#[derive(Clone, Debug)]
pub struct IntrinsicSizeOf(TypeKind);
impl IntrinsicFunction for IntrinsicSizeOf {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, _: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let instr = scope
            .block
            .build(Instr::Constant(reid_lib::ConstValue::U64(self.0.size_of() / 8)))
            .unwrap();
        Ok(StackValue(StackValueKind::Literal(instr), self.0.clone()))
    }
}

#[derive(Clone, Debug)]
pub struct IntrinsicMalloc(TypeKind);
impl IntrinsicFunction for IntrinsicMalloc {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, params: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let amount = params.get(0).unwrap();
        let function = scope
            .block
            .find_function(&format!("{}.{}", INTRINSIC_IDENT, MALLOC_IDENT))
            .unwrap();

        let sizeof = scope
            .block
            .build(Instr::Constant(ConstValue::U64(self.0.size_of() / 8)))
            .unwrap();
        let bytes = scope.block.build(Instr::Mul(sizeof, amount.instr())).unwrap();
        let instr = scope.block.build(Instr::FunctionCall(function, vec![bytes])).unwrap();
        Ok(StackValue(StackValueKind::Literal(instr), self.0.clone()))
    }
}

#[derive(Clone, Debug)]
pub struct IntrinsicNullPtr(TypeKind);
impl IntrinsicFunction for IntrinsicNullPtr {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, _: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let zero = scope.block.build(Instr::Constant(ConstValue::I8(0))).unwrap();
        let instr = scope
            .block
            .build(Instr::IntToPtr(
                zero,
                Type::Ptr(Box::new(self.0.get_type(scope.type_values))),
            ))
            .unwrap();
        Ok(StackValue(
            StackValueKind::Literal(instr),
            TypeKind::UserPtr(Box::new(self.0.clone())),
        ))
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
