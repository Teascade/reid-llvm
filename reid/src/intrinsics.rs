use reid_lib::Instr;

use crate::{
    codegen::{ErrorKind, Scope},
    mir::{BinaryOperator, BinopDefinition, FunctionDefinition, FunctionDefinitionKind, TypeKind},
};

#[derive(Debug, Clone, Copy)]
pub enum InstrinsicKind {
    IAdd,
}

fn intrinsic(
    name: &str,
    ret_ty: TypeKind,
    params: Vec<(&str, TypeKind)>,
    kind: InstrinsicKind,
) -> FunctionDefinition {
    FunctionDefinition {
        name: name.into(),
        is_pub: false,
        is_imported: false,
        return_type: ret_ty,
        parameters: params.into_iter().map(|(n, ty)| (n.into(), ty)).collect(),
        kind: FunctionDefinitionKind::Intrinsic(kind),
    }
}

fn intrinsic_binop(
    op: BinaryOperator,
    lhs: TypeKind,
    rhs: TypeKind,
    ret_ty: TypeKind,
    kind: InstrinsicKind,
) -> BinopDefinition {
    BinopDefinition {
        lhs: ("lhs".to_string(), lhs),
        op,
        rhs: ("rhs".to_owned(), rhs),
        return_type: ret_ty,
        fn_kind: FunctionDefinitionKind::Intrinsic(kind),
        meta: Default::default(),
    }
}

pub fn form_intrinsics() -> Vec<FunctionDefinition> {
    let mut intrinsics = Vec::new();

    intrinsics
}

pub fn form_intrinsic_binops() -> Vec<BinopDefinition> {
    let mut intrinsics = Vec::new();

    intrinsics.push(intrinsic_binop(
        BinaryOperator::Add,
        TypeKind::U32,
        TypeKind::U32,
        TypeKind::U32,
        InstrinsicKind::IAdd,
    ));

    intrinsics
}

impl InstrinsicKind {
    pub fn codegen<'ctx, 'a>(&self, scope: &mut Scope<'ctx, 'a>) -> Result<(), ErrorKind> {
        match self {
            InstrinsicKind::IAdd => {
                let lhs = scope.block.build(Instr::Param(0)).unwrap();
                let rhs = scope.block.build(Instr::Param(1)).unwrap();
                let add = scope.block.build(Instr::Add(lhs, rhs)).unwrap();
                scope
                    .block
                    .terminate(reid_lib::TerminatorKind::Ret(add))
                    .unwrap()
            }
        }
        Ok(())
    }
}
