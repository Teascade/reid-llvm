use reid_lib::{builder::InstructionValue, CmpPredicate, ConstValueKind, Instr, Type};

use crate::{
    codegen::{ErrorKind, StackValueKind},
    mir::{
        implement::TypeCategory, BinaryOperator, BinopDefinition, CmpOperator, FunctionDefinition,
        FunctionDefinitionKind, FunctionParam, TypeKind,
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

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum LLVMIntrinsicKind {
    Max(TypeKind),
    Min(TypeKind),
    Abs(TypeKind),
    Memcpy(TypeKind),
    Sqrt(TypeKind),
    PowI(TypeKind, TypeKind),
    Pow(TypeKind),
    Sin(TypeKind),
    Cos(TypeKind),
    Tan(TypeKind),
    ASin(TypeKind),
    ACos(TypeKind),
    ATan(TypeKind),
    ATan2(TypeKind),
    SinH(TypeKind),
    CosH(TypeKind),
    TanH(TypeKind),
    Log(TypeKind),
    Log2(TypeKind),
    Log10(TypeKind),
    Copysign(TypeKind),
    Floor(TypeKind),
    Ceil(TypeKind),
    Trunc(TypeKind),
    RoundEven(TypeKind),
    Round(TypeKind),
}

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
        signature_meta: Default::default(),
    });

    intrinsics
}

pub fn simple_intrinsic<T: Into<String> + Clone>(
    name: T,
    params: Vec<T>,
    ret: TypeKind,
    intrisic: LLVMIntrinsicKind,
) -> FunctionDefinition {
    FunctionDefinition {
        name: name.into(),
        linkage_name: None,
        is_pub: true,
        is_imported: false,
        return_type: ret.clone(),
        parameters: params
            .iter()
            .map(|p| FunctionParam::from(p.clone(), ret.clone()))
            .collect(),
        kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicLLVM(intrisic, ret.clone()))),
        source: None,
        signature_meta: Default::default(),
    }
}

pub fn get_intrinsic_assoc_functions(ty: &TypeKind) -> Vec<FunctionDefinition> {
    let mut intrinsics = Vec::new();
    if let TypeKind::Array(_, len) = ty {
        intrinsics.push(FunctionDefinition {
            name: "length".to_owned(),
            linkage_name: None,
            is_pub: true,
            is_imported: false,
            return_type: TypeKind::U64,
            parameters: vec![FunctionParam {
                name: String::from("self"),
                ty: TypeKind::Borrow(Box::new(ty.clone()), false),
                meta: Default::default(),
            }],
            kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicConst(*len))),
            source: None,
            signature_meta: Default::default(),
        });
    }
    if ty.category() == TypeCategory::Real {
        intrinsics.push(simple_intrinsic(
            "sqrt",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Sqrt(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "sin",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Sin(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "cos",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Cos(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "tan",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Tan(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "sinh",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::SinH(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "cosh",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::CosH(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "tanh",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::TanH(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "asin",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::ASin(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "acos",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::ACos(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "atan",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::ATan(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "atan2",
            vec!["self", "other"],
            ty.clone(),
            LLVMIntrinsicKind::ATan2(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "log",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Log(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "log2",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Log2(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "log10",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Log10(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "floor",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Floor(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "ceil",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Ceil(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "trunc",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Trunc(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "round",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::Round(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "even",
            vec!["self"],
            ty.clone(),
            LLVMIntrinsicKind::RoundEven(ty.clone()),
        ));
        intrinsics.push(simple_intrinsic(
            "pow",
            vec!["self", "exponent"],
            ty.clone(),
            LLVMIntrinsicKind::Pow(ty.clone()),
        ));
        intrinsics.push(FunctionDefinition {
            name: "powi".to_owned(),
            linkage_name: None,
            is_pub: true,
            is_imported: false,
            return_type: ty.clone(),
            parameters: vec![
                FunctionParam {
                    name: String::from("self"),
                    ty: ty.clone(),
                    meta: Default::default(),
                },
                FunctionParam {
                    name: String::from("exponent"),
                    ty: TypeKind::U32,
                    meta: Default::default(),
                },
            ],
            kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicLLVM(
                LLVMIntrinsicKind::PowI(ty.clone(), TypeKind::U32),
                ty.clone(),
            ))),
            source: None,
            signature_meta: Default::default(),
        });
    }
    match ty.category() {
        TypeCategory::Integer | TypeCategory::Real | TypeCategory::Bool => {
            intrinsics.push(simple_intrinsic(
                "max",
                vec!["self", "other"],
                ty.clone(),
                LLVMIntrinsicKind::Max(ty.clone()),
            ));
            intrinsics.push(simple_intrinsic(
                "min",
                vec!["self", "other"],
                ty.clone(),
                LLVMIntrinsicKind::Min(ty.clone()),
            ));
            if ty.signed() {
                intrinsics.push(FunctionDefinition {
                    name: "abs".to_owned(),
                    linkage_name: None,
                    is_pub: true,
                    is_imported: false,
                    return_type: ty.clone(),
                    parameters: vec![FunctionParam {
                        name: String::from("self"),
                        ty: ty.clone(),
                        meta: Default::default(),
                    }],
                    kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleUnaryInstr({
                        let ty = ty.clone();
                        |scope, param| {
                            let intrinsic = scope.get_intrinsic(LLVMIntrinsicKind::Abs(ty));
                            let constant = scope.block.build(Instr::Constant(ConstValueKind::Bool(false))).unwrap();
                            let value = scope
                                .block
                                .build(Instr::FunctionCall(intrinsic, vec![param, constant]))
                                .unwrap();
                            value
                        }
                    }))),
                    source: None,
                    signature_meta: Default::default(),
                });
            }
        }
        _ => {}
    }
    intrinsics.push(FunctionDefinition {
        name: "sizeof".to_owned(),
        linkage_name: None,
        is_pub: true,
        is_imported: false,
        return_type: TypeKind::U64,
        parameters: Vec::new(),
        kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSizeOf(ty.clone()))),
        source: None,
        signature_meta: Default::default(),
    });
    intrinsics.push(FunctionDefinition {
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
        signature_meta: Default::default(),
    });

    intrinsics.push(FunctionDefinition {
        name: "memcpy".to_owned(),
        linkage_name: None,
        is_pub: true,
        is_imported: false,
        return_type: TypeKind::Void,
        parameters: vec![
            FunctionParam {
                name: String::from("destination"),
                ty: TypeKind::UserPtr(Box::new(ty.clone())),
                meta: Default::default(),
            },
            FunctionParam {
                name: String::from("source"),
                ty: TypeKind::UserPtr(Box::new(ty.clone())),
                meta: Default::default(),
            },
            FunctionParam {
                name: String::from("length"),
                ty: TypeKind::U64,
                meta: Default::default(),
            },
        ],
        kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicMemcpy(ty.clone()))),
        source: None,
        signature_meta: Default::default(),
    });

    intrinsics.push(FunctionDefinition {
        name: "null".to_owned(),
        linkage_name: None,
        is_pub: true,
        is_imported: false,
        return_type: TypeKind::UserPtr(Box::new(ty.clone())),
        parameters: Vec::new(),
        kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicNullPtr(ty.clone()))),
        source: None,
        signature_meta: Default::default(),
    });

    intrinsics.push(FunctionDefinition {
        name: "is_null".to_owned(),
        linkage_name: None,
        is_pub: true,
        is_imported: false,
        return_type: TypeKind::Bool,
        parameters: vec![FunctionParam {
            name: "value".to_string(),
            ty: TypeKind::UserPtr(Box::new(ty.clone())),
            meta: Default::default(),
        }],
        kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicIsNull)),
        source: None,
        signature_meta: Default::default(),
    });

    intrinsics
}

pub fn get_intrinsic_assoc_func(ty: &TypeKind, name: &str) -> Option<FunctionDefinition> {
    get_intrinsic_assoc_functions(ty).into_iter().find(|f| f.name == name)
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
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleBinaryInstr(fun))),
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
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(IntrinsicSimpleBinaryInstr(fun))),
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
            intrinsics.push(complex_binop_def(BitshiftRight, &ty, &ty, |scope, lhs, rhs| {
                scope.block.build(Instr::ShiftRightArithmetic(lhs, rhs)).unwrap()
            }));
        } else {
            intrinsics.push(complex_binop_def(BitshiftRight, &ty, &ty, |scope, lhs, rhs| {
                scope.block.build(Instr::ShiftRightLogical(lhs, rhs)).unwrap()
            }));
        }
        intrinsics.push(complex_binop_def(BitshiftLeft, &ty, &ty, |scope, lhs, rhs| {
            scope.block.build(Instr::ShiftLeft(lhs, rhs)).unwrap()
        }));
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
        intrinsics.push(simple_binop_def(Mod, &ty, {
            let ty = ty.clone();
            |scope, lhs, rhs| {
                let div = scope.block.build(Instr::FDiv(lhs, rhs)).unwrap();
                let fun = scope.get_intrinsic(LLVMIntrinsicKind::Trunc(ty));
                let div_truncated = scope.block.build(Instr::FunctionCall(fun, vec![div])).unwrap();
                let mul = scope.block.build(Instr::FMul(rhs, div_truncated)).unwrap();
                scope.block.build(Instr::FSub(lhs, mul)).unwrap()
            }
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
pub struct IntrinsicSimpleUnaryInstr<T>(T)
where
    T: FnOnce(&mut Scope, InstructionValue) -> InstructionValue;

impl<T> std::fmt::Debug for IntrinsicSimpleUnaryInstr<T>
where
    T: FnOnce(&mut Scope, InstructionValue) -> InstructionValue,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("IntrinsicSimpleUnaryInstr").finish()
    }
}

impl<T: Clone> IntrinsicFunction for IntrinsicSimpleUnaryInstr<T>
where
    T: FnOnce(&mut Scope, InstructionValue) -> InstructionValue,
{
    fn codegen<'b, 'c>(&self, scope: &mut Scope<'b, 'c>, params: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let param = params.get(0).unwrap();
        let instr = self.clone().0(scope, param.instr());
        Ok(StackValue(StackValueKind::Literal(instr), param.1.clone()))
    }
}

#[derive(Clone)]
pub struct IntrinsicSimpleBinaryInstr<T>(T)
where
    T: FnOnce(&mut Scope, InstructionValue, InstructionValue) -> InstructionValue;
intrinsic_debug!(IntrinsicSimpleBinaryInstr<T>, "IntrinsicSimpleBinaryInstr");

impl<T: Clone> IntrinsicFunction for IntrinsicSimpleBinaryInstr<T>
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
            .build(Instr::Constant(reid_lib::ConstValueKind::U64(
                self.0.size_of(&scope.type_map) / 8,
            )))
            .unwrap();
        Ok(StackValue(StackValueKind::Literal(instr), self.0.clone()))
    }
}

#[derive(Clone, Debug)]
pub struct IntrinsicMemcpy(TypeKind);
impl IntrinsicFunction for IntrinsicMemcpy {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, params: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let dest = params.get(0).unwrap();
        let src = params.get(1).unwrap();
        let length = params.get(2).unwrap();
        let intrinsic = scope.get_intrinsic(LLVMIntrinsicKind::Memcpy(TypeKind::UserPtr(Box::new(self.0.clone()))));

        let sizeof = scope
            .block
            .build(Instr::Constant(ConstValueKind::U64(
                self.0.size_of(&scope.type_map) / 8,
            )))
            .unwrap();
        let bytes = scope.block.build(Instr::Mul(sizeof, length.instr())).unwrap();

        let params = vec![
            dest.instr(),
            src.instr(),
            bytes,
            scope.block.build(Instr::Constant(ConstValueKind::Bool(false))).unwrap(),
        ];
        let value = scope.block.build(Instr::FunctionCall(intrinsic, params)).unwrap();
        Ok(StackValue(StackValueKind::Literal(value), TypeKind::Void))
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
            .build(Instr::Constant(ConstValueKind::U64(
                self.0.size_of(&scope.type_map) / 8,
            )))
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
        let zero = scope.block.build(Instr::Constant(ConstValueKind::I8(0))).unwrap();
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

#[derive(Clone, Debug)]
pub struct IntrinsicIsNull;
impl IntrinsicFunction for IntrinsicIsNull {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, params: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let val = params.get(0).unwrap().instr();
        let instr = scope.block.build(Instr::IsNull(val)).unwrap();
        Ok(StackValue(StackValueKind::Literal(instr), TypeKind::Bool))
    }
}

#[derive(Clone, Debug)]
pub struct IntrinsicConst(u64);
impl IntrinsicFunction for IntrinsicConst {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, _: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let zero = scope.block.build(Instr::Constant(ConstValueKind::U64(self.0))).unwrap();
        Ok(StackValue(StackValueKind::Literal(zero), TypeKind::U64))
    }
}

#[derive(Clone, Debug)]
pub struct IntrinsicLLVM(LLVMIntrinsicKind, TypeKind);
impl IntrinsicFunction for IntrinsicLLVM {
    fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>, params: &[StackValue]) -> Result<StackValue, ErrorKind> {
        let intrinsic = scope.get_intrinsic(self.0.clone());
        let value = scope
            .block
            .build(Instr::FunctionCall(
                intrinsic,
                params.iter().map(|p| p.instr()).collect(),
            ))
            .unwrap();

        Ok(StackValue(StackValueKind::Literal(value), self.1.clone()))
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
