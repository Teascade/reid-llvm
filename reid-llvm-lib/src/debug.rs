//! Debug implementations for relevant types

use std::{
    fmt::{Debug, Write},
    marker::PhantomData,
};

use crate::{CmpPredicate, Instr, InstructionData, TerminatorKind, builder::*};

impl Debug for Builder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.get_modules().borrow().iter());
        Ok(())
    }
}

pub struct PrintableModule<'ctx> {
    pub phantom: PhantomData<&'ctx ()>,
    pub module: ModuleHolder,
}

impl<'ctx> Debug for PrintableModule<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.module.fmt(f)
    }
}

impl Debug for ModuleHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("{}({:#?}) ", self.data.name, self.value))
            .field(&self.functions)
            .finish()
    }
}

impl Debug for FunctionHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!(
            "{}({:?}) -> {:?} ",
            self.data.name, self.data.params, self.data.ret
        ))
        .field(&self.blocks)
        .finish()
    }
}

impl Debug for BlockHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let deleted = if self.data.deleted { " (deleted)" } else { "" };
        f.debug_tuple(&format!(
            "{}[{:?}]{} ",
            &self.data.name, &self.value, deleted
        ))
        .field(&self.instructions)
        .field(&self.data.terminator)
        .finish()
    }
}

impl Debug for InstructionHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)?;
        f.write_str(" = ")?;
        self.data.fmt(f)
    }
}

impl Debug for InstructionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Debug for ModuleValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "M[{:0>2}]", self.0)
    }
}

impl Debug for FunctionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "F[{:0>2}-{:0>2}]", &self.0.0, self.1)
    }
}

impl Debug for BlockValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "B[{:0>2}-{:0>2}-{:0>2}]", &self.0.0.0, &self.0.1, self.1)
    }
}

impl Debug for InstructionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "I[{:0>2}-{:0>2}-{:0>2}-{:0>2}]",
            &self.0.0.0.0, &self.0.0.1, &self.0.1, self.1
        )
    }
}

impl Debug for TypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ty[{:0>2}-{:0>2}]", &self.0.0, self.1)
    }
}

impl Debug for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Param(nth) => fmt_call(f, &"Param", &nth),
            Instr::Constant(c) => c.fmt(f),
            Instr::Add(lhs, rhs) => fmt_binop(f, lhs, &"+", rhs),
            Instr::Sub(lhs, rhs) => fmt_binop(f, lhs, &"-", rhs),
            Instr::Mult(lhs, rhs) => fmt_binop(f, lhs, &"*", rhs),
            Instr::And(lhs, rhs) => fmt_binop(f, lhs, &"&&", rhs),
            Instr::Phi(val) => fmt_call(f, &"Phi", &val),
            Instr::ICmp(cmp, lhs, rhs) => fmt_binop(f, lhs, cmp, rhs),
            Instr::FunctionCall(fun, params) => fmt_call(f, fun, params),
            Instr::Alloca(name, ty) => write!(f, "alloca<{:?}>({})", ty, name),
            Instr::Load(val, ty) => write!(f, "load<{:?}>({:?})", ty, val),
            Instr::Store(ptr, val) => write!(f, "store({:?} = {:?})", ptr, val),
            Instr::ArrayAlloca(ty, instruction_value) => {
                write!(f, "array_alloca<{:?}>({:?})", ty, instruction_value)
            }
            Instr::GetElemPtr(instruction_value, items) => fmt_index(
                f,
                instruction_value,
                &items
                    .iter()
                    .map(|expr| format!("{:?}", expr))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Instr::GetStructElemPtr(instruction_value, index) => {
                fmt_index(f, instruction_value, &index.to_string())
            }
        }
    }
}

fn fmt_binop(
    f: &mut std::fmt::Formatter<'_>,
    lhs: &impl std::fmt::Debug,
    op: &impl std::fmt::Debug,
    rhs: &impl std::fmt::Debug,
) -> std::fmt::Result {
    lhs.fmt(f)?;
    f.write_char(' ')?;
    op.fmt(f)?;
    f.write_char(' ')?;
    rhs.fmt(f)
}

fn fmt_call(
    f: &mut std::fmt::Formatter<'_>,
    fun: &impl std::fmt::Debug,
    params: &impl std::fmt::Debug,
) -> std::fmt::Result {
    fun.fmt(f)?;
    f.write_char('(')?;
    params.fmt(f)?;
    f.write_char(')')
}

fn fmt_index(
    f: &mut std::fmt::Formatter<'_>,
    fun: &impl std::fmt::Debug,
    params: &impl std::fmt::Debug,
) -> std::fmt::Result {
    fun.fmt(f)?;
    f.write_char('[')?;
    params.fmt(f)?;
    f.write_char(']')
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
            Self::Ret(val) => {
                write!(f, "Ret ")?;
                val.fmt(f)
            }
            Self::RetVoid => write!(f, "Void Ret"),
            Self::Br(val) => {
                write!(f, "Br ")?;
                val.fmt(f)
            }
            Self::CondBr(cond, b1, b2) => {
                write!(f, "CondBr ")?;
                cond.fmt(f)?;
                write!(f, " ? ")?;
                b1.fmt(f)?;
                write!(f, " : ")?;
                b2.fmt(f)?;
                Ok(())
            }
        }
    }
}
