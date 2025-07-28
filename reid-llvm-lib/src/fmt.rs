//! Debug implementations for relevant types

use std::{
    fmt::{Debug, Display, Write},
    marker::PhantomData,
};

use crate::{
    CmpPredicate, Context, Instr, InstructionData, TerminatorKind,
    builder::*,
    debug_information::{
        DebugArrayType, DebugBasicType, DebugFieldType, DebugInformation, DebugLocalVariable, DebugLocation,
        DebugLocationValue, DebugMetadata, DebugMetadataValue, DebugParamVariable, DebugPointerType, DebugPosition,
        DebugRecordKind, DebugScopeValue, DebugStructType, DebugSubprogramType, DebugTypeData, DebugTypeHolder,
        DebugTypeValue,
    },
    pad_adapter::PadAdapter,
};

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.builder, f)
    }
}

impl Display for Builder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Producer: {}", self.producer)?;
        for module in self.modules.borrow().iter() {
            if module.data.is_main {
                write!(f, "main ")?;
            }
            writeln!(f, "{} ({:?}) {{", module.data.name, module.value)?;
            for function in &module.functions {
                let mut state = Default::default();
                let mut inner = PadAdapter::wrap(f, &mut state);
                function.builder_fmt(&mut inner, self, &module.debug_information)?;
            }
            writeln!(f, "}}")?;
        }
        Ok(())
    }
}

impl FunctionHolder {
    fn builder_fmt(
        &self,
        f: &mut impl std::fmt::Write,
        builder: &Builder,
        debug: &Option<DebugInformation>,
    ) -> std::fmt::Result {
        if self.data.flags.is_imported {
            write!(f, "imported ")?;
        }
        if self.data.flags.is_extern {
            write!(f, "extern ")?;
        }
        if self.data.flags.is_pub {
            write!(f, "pub ")?;
        }
        if self.data.flags.is_main {
            write!(f, "main ")?;
        }
        let params = self
            .data
            .params
            .iter()
            .map(|p| format!("{:?}", p))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "fn {}({}) -> {:?} ", self.data.name, params, self.data.ret)?;

        writeln!(f, "{{")?;
        let mut state = Default::default();
        let mut inner = PadAdapter::wrap(f, &mut state);
        writeln!(inner, "(Value = {:?}) ", self.value)?;
        if let Some(debug) = &self.debug_info {
            writeln!(inner, "(Debug = {:?})", debug)?;
        }

        for block in &self.blocks {
            let mut state = Default::default();
            let mut inner = PadAdapter::wrap(&mut inner, &mut state);
            block.builder_fmt(&mut inner, builder, debug)?;
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}

impl BlockHolder {
    fn builder_fmt(
        &self,
        f: &mut impl std::fmt::Write,
        builder: &Builder,
        debug: &Option<DebugInformation>,
    ) -> std::fmt::Result {
        if self.data.deleted {
            write!(f, "deleted ")?;
        }
        writeln!(f, "{} ({:?}):", self.data.name, self.value)?;

        let mut state = Default::default();
        let mut inner = PadAdapter::wrap(f, &mut state);

        for instr in &self.instructions {
            instr.builder_fmt(&mut inner, builder, debug)?;
        }

        if let Some(terminator) = &self.data.terminator {
            terminator.builder_fmt(&mut inner, builder, debug)?;
        }
        if let Some(location) = &self.data.terminator_location {
            writeln!(inner, "  ^  (At {}) ", debug.as_ref().unwrap().get_location(location))?;
        }

        Ok(())
    }
}

impl InstructionHolder {
    fn builder_fmt(
        &self,
        f: &mut impl std::fmt::Write,
        _builder: &Builder,
        debug: &Option<DebugInformation>,
    ) -> std::fmt::Result {
        if let Some(record) = &self.record {
            let kind = match record.kind {
                DebugRecordKind::Declare(instruction_value) => {
                    format!("= {:?} (Assign)", instruction_value)
                }
                DebugRecordKind::Value(instruction_value) => {
                    format!("= {:?} (Value)", instruction_value)
                }
            };

            if let Some(debug) = debug {
                writeln!(f, "  (Debug {} {})", record.variable.hr(debug), kind)?;
            }
        }
        writeln!(f, "{:?} ({}) = {:?} ", self.value, self.name, self.data.kind)?;
        if let Some(debug) = debug {
            if let Some(location) = &self.data.location {
                writeln!(f, "  ^  (At {}) ", debug.get_location(location))?;
            }
            if let Some(meta) = self.data.meta {
                writeln!(f, "  ^  (Meta {}) ", meta.hr(debug))?;
            }
        }
        writeln!(f)?;

        Ok(())
    }
}

impl TerminatorKind {
    fn builder_fmt(
        &self,
        f: &mut impl std::fmt::Write,
        _builder: &Builder,
        _debug: &Option<DebugInformation>,
    ) -> std::fmt::Result {
        match self {
            TerminatorKind::Ret(instr) => writeln!(f, "ret {:?}", instr),
            TerminatorKind::RetVoid => writeln!(f, "ret void"),
            TerminatorKind::Br(block) => writeln!(f, "br {:?}", block),
            TerminatorKind::CondBr(instr, lhs, rhs) => {
                writeln!(f, "condbr {:?}, {:?} or {:?}", instr, lhs, rhs)
            }
        }
    }
}

impl DebugMetadataValue {
    fn hr(&self, debug: &DebugInformation) -> String {
        let kind = match debug.get_metadata(*self) {
            DebugMetadata::ParamVar(DebugParamVariable { name, arg_idx, ty, .. }) => {
                format!("param {} (idx {}) (type {:?}) ", name, arg_idx, ty)
            }
            DebugMetadata::LocalVar(DebugLocalVariable { name, ty, .. }) => {
                format!("var {} (type {:?}) ", name, ty)
            }
            DebugMetadata::VarAssignment => todo!(),
        };
        format!("{} at {}", kind, debug.get_metadata_location(*self))
    }
}

impl Display for DebugLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} on scope {:?}", self.pos, self.scope)
    }
}

impl Display for DebugPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, col {}", self.line, self.column)
    }
}

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
        Debug::fmt(&self.module, f)
    }
}

impl Debug for ModuleHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("{}({:#?}) ", self.data.name, self.value))
            .field(&self.functions)
            // .field(&self.debug_information)
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
        f.debug_tuple(&format!("{}[{:?}]{} ", &self.data.name, &self.value, deleted))
            .field(&self.instructions)
            .field(&self.data.terminator)
            .field(&self.data.terminator_location)
            .finish()
    }
}

impl Debug for InstructionHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)?;
        write!(f, " ({})", self.name)?;
        f.write_str(" = ")?;
        self.data.fmt(f)
    }
}

impl Debug for InstructionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)?;
        if let Some(location) = &self.location {
            write!(f, " ({:?})", location)?;
        }
        Ok(())
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
        write!(f, "%{}.{}.{}.{}", self.0.0.0.0, self.0.0.1, self.0.1, self.1)
    }
}

// impl Debug for InstructionValue {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "I[{:0>2}-{:0>2}-{:0>2}-{:0>2}]",
//             &self.0.0.0.0, &self.0.0.1, &self.0.1, self.1
//         )
//     }
// }

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
            Instr::FAdd(lhs, rhs) => fmt_binop(f, lhs, &"+", rhs),
            Instr::Sub(lhs, rhs) => fmt_binop(f, lhs, &"-", rhs),
            Instr::FSub(lhs, rhs) => fmt_binop(f, lhs, &"-", rhs),
            Instr::Mul(lhs, rhs) => fmt_binop(f, lhs, &"*", rhs),
            Instr::FMul(lhs, rhs) => fmt_binop(f, lhs, &"*", rhs),
            Instr::UDiv(lhs, rhs) => fmt_binop(f, lhs, &"/", rhs),
            Instr::SDiv(lhs, rhs) => fmt_binop(f, lhs, &"/", rhs),
            Instr::FDiv(lhs, rhs) => fmt_binop(f, lhs, &"/", rhs),
            Instr::URem(lhs, rhs) => fmt_binop(f, lhs, &"%", rhs),
            Instr::SRem(lhs, rhs) => fmt_binop(f, lhs, &"%", rhs),
            Instr::FRem(lhs, rhs) => fmt_binop(f, lhs, &"%", rhs),
            Instr::And(lhs, rhs) => fmt_binop(f, lhs, &"&&", rhs),
            Instr::Phi(val) => fmt_call(f, &"Phi", &val),
            Instr::ICmp(cmp, lhs, rhs) => fmt_binop(f, lhs, cmp, rhs),
            Instr::FCmp(cmp, lhs, rhs) => fmt_binop(f, lhs, cmp, rhs),
            Instr::FunctionCall(fun, params) => fmt_call(f, fun, params),
            Instr::Alloca(ty) => write!(f, "alloca<{:?}>", ty),
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
                write!(f, "GEP(")?;
                fmt_index(f, instruction_value, &index.to_string())?;
                write!(f, ")")
            }
            Instr::ExtractValue(instruction_value, index) => fmt_index(f, instruction_value, &index.to_string()),
            Instr::Trunc(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::ZExt(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::SExt(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::FPTrunc(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::FPExt(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::FPToUI(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::FPToSI(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::UIToFP(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::SIToFP(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::PtrToInt(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::IntToPtr(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::BitCast(instr_val, ty) => {
                write!(f, "{:?} to {:?} ({})", instr_val, ty, self.default_name())
            }
            Instr::Or(lhs, rhs) => fmt_binop(f, lhs, &"||", rhs),
            Instr::XOr(lhs, rhs) => fmt_binop(f, lhs, &"^", rhs),
            Instr::ShiftRightLogical(lhs, rhs) => fmt_binop(f, lhs, &">>l", rhs),
            Instr::ShiftRightArithmetic(lhs, rhs) => fmt_binop(f, lhs, &">>a", rhs),
            Instr::ShiftLeft(lhs, rhs) => fmt_binop(f, lhs, &"<<", rhs),
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

impl Debug for DebugTypeHolder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("DebugTypeHolder {:?}", self.value))
            .field(&self.data)
            .finish()
    }
}

impl Debug for DebugTypeData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DebugTypeData::Basic(ty) => Debug::fmt(ty, f),
            DebugTypeData::Subprogram(ty) => Debug::fmt(ty, f),
            DebugTypeData::Pointer(ty) => Debug::fmt(ty, f),
            DebugTypeData::Array(ty) => Debug::fmt(ty, f),
            DebugTypeData::Struct(ty) => Debug::fmt(ty, f),
        }
    }
}

impl Debug for DebugBasicType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("BasicType")
            .field(&self.name)
            .field(&self.size_bits)
            .field(&self.encoding)
            .field(&self.flags)
            .finish()
    }
}

impl Debug for DebugStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Struct")
            .field("name", &self.name)
            .field("scope", &self.scope)
            .field("pos", &self.pos)
            .field("size_bit", &self.size_bits)
            .field("flags", &self.flags)
            .field("elements", &self.fields)
            .finish()
    }
}

impl Debug for DebugFieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Field({})", self.name))
            .field("scope", &self.scope)
            .field("pos", &self.pos)
            .field("size_bits", &self.size_bits)
            .field("offset", &self.offset)
            .field("flags", &self.flags)
            .field("ty", &self.ty)
            .finish()
    }
}

impl Debug for DebugSubprogramType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Subprogram")
            .field(&self.parameters)
            .field(&self.flags)
            .finish()
    }
}

impl Debug for DebugPointerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("Pointer<{:?}>({})", self.pointee, self.name))
            .field(&self.size_bits)
            .finish()
    }
}

impl Debug for DebugArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Array<{:?}>[{}]", self.element_type, self.length))
            .field("size_bits", &self.size_bits)
            .field("align_bits", &self.align_bits)
            .finish()
    }
}

impl Debug for DebugMetadataValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Meta[{}]", self.0)
    }
}

impl Debug for DebugScopeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Scope[{}]",
            self.0.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ")
        )
    }
}

impl Debug for DebugTypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type[{}]", self.0)
    }
}

impl Debug for DebugLocationValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value[{:?}][{}]", self.0, self.1)
    }
}

impl Debug for DebugLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} on scope {:?}", self.pos, self.scope)
    }
}

impl Debug for DebugPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ln {}, col {}", self.line, self.column)
    }
}
