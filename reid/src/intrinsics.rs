use reid_lib::{builder::InstructionValue, Instr};

use crate::{
    codegen::{ErrorKind, Scope, StackValue, StackValueKind},
    mir::{BinaryOperator, BinopDefinition, FunctionDefinition, FunctionDefinitionKind, TypeKind},
};

fn intrinsic(
    name: &str,
    ret_ty: TypeKind,
    params: Vec<(&str, TypeKind)>,
    fun: impl IntrinsicFunction + 'static,
) -> FunctionDefinition {
    FunctionDefinition {
        name: name.into(),
        is_pub: false,
        is_imported: false,
        return_type: ret_ty,
        parameters: params.into_iter().map(|(n, ty)| (n.into(), ty)).collect(),
        kind: FunctionDefinitionKind::Intrinsic(Box::new(fun)),
    }
}

fn intrinsic_binop(
    op: BinaryOperator,
    lhs: TypeKind,
    rhs: TypeKind,
    ret_ty: TypeKind,
    fun: impl IntrinsicFunction + 'static,
) -> BinopDefinition {
    BinopDefinition {
        lhs: ("lhs".to_string(), lhs),
        op,
        rhs: ("rhs".to_owned(), rhs),
        return_type: ret_ty,
        fn_kind: FunctionDefinitionKind::Intrinsic(Box::new(fun)),
        meta: Default::default(),
    }
}

pub fn form_intrinsics() -> Vec<FunctionDefinition> {
    let intrinsics = Vec::new();

    intrinsics
}

pub fn form_intrinsic_binops() -> Vec<BinopDefinition> {
    let mut intrinsics = Vec::new();

    intrinsics
}

pub trait IntrinsicFunction: std::fmt::Debug {
    fn codegen<'ctx, 'a>(
        &self,
        scope: &mut Scope<'ctx, 'a>,
        params: &[InstructionValue],
    ) -> Result<StackValue, ErrorKind>;
}

#[derive(Debug, Clone)]
pub struct IntrinsicIAdd(TypeKind);

impl IntrinsicFunction for IntrinsicIAdd {
    fn codegen<'ctx, 'a>(
        &self,
        scope: &mut Scope<'ctx, 'a>,
        params: &[InstructionValue],
    ) -> Result<StackValue, ErrorKind> {
        let lhs = params.get(0).unwrap();
        let rhs = params.get(1).unwrap();
        let add = scope.block.build(Instr::Add(*lhs, *rhs)).unwrap();
        Ok(StackValue(StackValueKind::Literal(add), self.0.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct IntrinsicUDiv(TypeKind);

impl IntrinsicFunction for IntrinsicUDiv {
    fn codegen<'ctx, 'a>(
        &self,
        scope: &mut Scope<'ctx, 'a>,
        params: &[InstructionValue],
    ) -> Result<StackValue, ErrorKind> {
        let lhs = params.get(0).unwrap();
        let rhs = params.get(1).unwrap();
        let add = scope.block.build(Instr::UDiv(*lhs, *rhs)).unwrap();
        Ok(StackValue(StackValueKind::Literal(add), self.0.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct IntrinsicUMod(TypeKind);

impl IntrinsicFunction for IntrinsicUMod {
    fn codegen<'ctx, 'a>(
        &self,
        scope: &mut Scope<'ctx, 'a>,
        params: &[InstructionValue],
    ) -> Result<StackValue, ErrorKind> {
        let lhs = params.get(0).unwrap();
        let rhs = params.get(1).unwrap();
        let div = scope.block.build(Instr::UDiv(*lhs, *rhs)).unwrap();
        let mul = scope.block.build(Instr::Mul(*rhs, div)).unwrap();
        let sub = scope.block.build(Instr::Sub(*lhs, mul)).unwrap();
        Ok(StackValue(StackValueKind::Literal(sub), self.0.clone()))
    }
}
