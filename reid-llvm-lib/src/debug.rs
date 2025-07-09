use std::fmt::Debug;

use crate::{CmpPredicate, InstructionData, InstructionKind, TerminatorKind, builder::*};

impl Debug for ModuleHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("Module({})", self.data.name))
            .field(&self.value)
            .field(&self.functions)
            .finish()
    }
}

impl Debug for FunctionHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!(
            "{}({:?}) -> {:?}",
            self.data.name, self.data.params, self.data.ret
        ))
        .field(&self.blocks)
        .finish()
    }
}

impl Debug for BlockHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("Block({})", self.data.name))
            .field(&self.value)
            .field(&self.instructions)
            .field(&self.data.terminator)
            .finish()
    }
}

impl Debug for InstructionHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} = {:?}", &self.value, &self.data)
    }
}

impl Debug for InstructionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Debug for ModuleValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "M[{}]", &self.0)
    }
}

impl Debug for FunctionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "F[{}, {}]", &self.0.0, &self.1,)
    }
}

impl Debug for BlockValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "B[{}, {}, {}]", &self.0.0.0, &self.0.1, self.1)
    }
}

impl Debug for InstructionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "I<{}-{}-{}-{}>",
            &self.0.0.0.0, &self.0.0.1, &self.0.1, self.1
        )
    }
}

impl Debug for InstructionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Param(nth) => write!(f, "Param({})", &nth),
            Self::Constant(c) => write!(f, "{:?}", &c),
            Self::Add(lhs, rhs) => write!(f, "{:?} + {:?}", &lhs, &rhs),
            Self::Sub(lhs, rhs) => write!(f, "{:?} + {:?}", &lhs, &rhs),
            Self::Phi(val) => write!(f, "Phi: {:?}", &val),
            Self::ICmp(cmp, lhs, rhs) => write!(f, "{:?} {:?} {:?}", &lhs, &cmp, &rhs),
            Self::FunctionCall(fun, params) => write!(f, "{:?}({:?})", &fun, &params),
        }
    }
}

impl Debug for CmpPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LT => write!(f, "<"),
            Self::GT => write!(f, ">"),
            Self::LE => write!(f, "<="),
            Self::GE => write!(f, ">="),
            Self::EQ => write!(f, "=="),
            Self::NE => write!(f, "!="),
        }
    }
}

impl Debug for TerminatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ret(val) => write!(f, "Ret {:?}", &val),
            Self::Branch(val) => write!(f, "Br {:?}", &val),
            Self::CondBr(cond, b1, b2) => write!(f, "CondBr {:?} ? {:?} : {:?}", &cond, &b1, &b2),
        }
    }
}
