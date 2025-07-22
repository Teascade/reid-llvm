//! Reid LLVM Lib is an ergonomic Rust'y API which is used to produce a
//! Low-Level IR (LLIR) using [`Context`] and [`Builder`]. This Builder can then
//! be used at the end to compile said LLIR into LLVM IR.

use std::{fmt::Debug, marker::PhantomData};

use builder::{BlockValue, Builder, FunctionValue, InstructionValue, ModuleValue, TypeValue};
use debug_information::{
    DebugFileData, DebugInformation, DebugLocationValue, DebugMetadataValue, DebugProgramValue,
    InstructionDebugRecordData,
};
use fmt::PrintableModule;
use util::match_types;

pub mod builder;
pub mod compile;
pub mod debug_information;
mod fmt;
mod pad_adapter;
mod util;

#[derive(Debug)]
pub struct Context {
    builder: Builder,
}

impl Context {
    pub fn new<T: Into<String>>(producer: T) -> Context {
        Context {
            builder: Builder::new(producer.into()),
        }
    }

    pub fn module<'ctx>(&'ctx self, name: &str, main: bool) -> Module<'ctx> {
        let value = self.builder.add_module(ModuleData {
            name: name.to_owned(),
            is_main: main,
        });
        Module {
            phantom: PhantomData,
            builder: self.builder.clone(),
            value,
            debug_info: None,
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct ModuleData {
    name: String,
    is_main: bool,
}

#[derive(Clone)]
pub struct Module<'ctx> {
    phantom: PhantomData<&'ctx ()>,
    builder: Builder,
    value: ModuleValue,
    debug_info: Option<DebugInformation>,
}

impl<'ctx> Module<'ctx> {
    pub fn function(
        &self,
        name: &str,
        ret: Type,
        params: Vec<Type>,
        flags: FunctionFlags,
    ) -> Function<'ctx> {
        unsafe {
            Function {
                phantom: PhantomData,
                builder: self.builder.clone(),
                value: self.builder.add_function(
                    &self.value,
                    FunctionData {
                        name: name.to_owned(),
                        ret,
                        params,
                        flags,
                        debug: None,
                    },
                ),
            }
        }
    }

    pub fn custom_type(&self, ty: CustomTypeKind) -> TypeValue {
        unsafe {
            let (name, kind) = match &ty {
                CustomTypeKind::NamedStruct(NamedStruct(name, _)) => (name.clone(), ty),
            };
            self.builder.add_type(&self.value, TypeData { name, kind })
        }
    }

    pub fn value(&self) -> ModuleValue {
        self.value
    }

    pub fn as_printable(&self) -> PrintableModule<'ctx> {
        PrintableModule {
            phantom: PhantomData,
            module: self.builder.find_module(self.value),
        }
    }

    pub fn create_debug_info(
        &mut self,
        file: DebugFileData,
    ) -> (DebugInformation, DebugProgramValue) {
        let (debug_info, program_value) = DebugInformation::from_file(file);
        self.debug_info = Some(debug_info.clone());
        (debug_info, program_value)
    }

    pub fn get_debug_info(&self) -> &Option<DebugInformation> {
        &self.debug_info
    }
}

impl<'ctx> Drop for Module<'ctx> {
    fn drop(&mut self) {
        if let Some(debug_info) = self.debug_info.take() {
            self.builder.set_debug_information(&self.value, debug_info);
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct FunctionData {
    name: String,
    ret: Type,
    params: Vec<Type>,
    flags: FunctionFlags,
    debug: Option<DebugProgramValue>,
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct FunctionFlags {
    pub is_extern: bool,
    pub is_main: bool,
    pub is_pub: bool,
    pub is_imported: bool,
}

impl Default for FunctionFlags {
    fn default() -> FunctionFlags {
        FunctionFlags {
            is_extern: false,
            is_main: false,
            is_pub: true,
            is_imported: false,
        }
    }
}

pub struct Function<'ctx> {
    phantom: PhantomData<&'ctx ()>,
    builder: Builder,
    value: FunctionValue,
}

impl<'ctx> Function<'ctx> {
    pub fn block(&self, name: &str) -> Block<'ctx> {
        unsafe {
            Block {
                phantom: PhantomData,
                builder: self.builder.clone(),
                value: self.builder.add_block(
                    &self.value,
                    BlockData {
                        name: name.to_owned(),
                        terminator: None,
                        terminator_location: None,
                        deleted: false,
                    },
                ),
            }
        }
    }

    pub fn set_debug(&self, subprogram: DebugProgramValue) {
        unsafe {
            self.builder.set_debug_subprogram(&self.value, subprogram);
        }
    }

    pub fn value(&self) -> FunctionValue {
        self.value
    }
}

#[derive(Debug, Clone, Hash)]
pub struct BlockData {
    name: String,
    terminator: Option<TerminatorKind>,
    terminator_location: Option<DebugLocationValue>,
    deleted: bool,
}

#[derive(Clone)]
pub struct Block<'builder> {
    phantom: PhantomData<&'builder ()>,
    builder: Builder,
    value: BlockValue,
}

impl Instr {
    pub fn default_name(&self) -> &str {
        match self {
            Instr::Param(_) => "param",
            Instr::Constant(_) => "const1",
            Instr::Add(..) => "add",
            Instr::FAdd(..) => "fadd",
            Instr::Sub(..) => "sub",
            Instr::FSub(..) => "fsub",
            Instr::Mul(..) => "mul",
            Instr::FMul(..) => "fmul",
            Instr::UDiv(..) => "udiv",
            Instr::SDiv(..) => "sdiv",
            Instr::FDiv(..) => "fdiv",
            Instr::URem(..) => "urem",
            Instr::SRem(..) => "srem",
            Instr::FRem(..) => "frem",
            Instr::And(..) => "and",
            Instr::Phi(_) => "phi",
            Instr::Alloca(_) => "alloca",
            Instr::Load(_, _) => "load",
            Instr::Store(..) => "store",
            Instr::ArrayAlloca(_, _) => "arrayalloca",
            Instr::GetElemPtr(..) => "getelemptr",
            Instr::GetStructElemPtr(..) => "getstructelemptr",
            Instr::ExtractValue(..) => "extractvalue",
            Instr::ICmp(..) => "icmp",
            Instr::FunctionCall(..) => "call",
            Instr::FCmp(_, _, _) => "fcmp",
            Instr::Trunc(_, _) => "trunc",
            Instr::ZExt(_, _) => "zext",
            Instr::SExt(_, _) => "sext",
            Instr::FPTrunc(_, _) => "fptrunc",
            Instr::FPExt(_, _) => "pfext",
            Instr::FPToUI(_, _) => "fptoui",
            Instr::FPToSI(_, _) => "fptosi",
            Instr::UIToFP(_, _) => "uitofp",
            Instr::SIToFP(_, _) => "sitofp",
            Instr::PtrToInt(_, _) => "ptrtoint",
            Instr::IntToPtr(_, _) => "inttoptr",
            Instr::BitCast(_, _) => "bitcast",
        }
    }
}

impl<'builder> Block<'builder> {
    pub fn build_named<T: Into<String>>(
        &mut self,
        name: T,
        instruction: Instr,
    ) -> Result<InstructionValue, ()> {
        unsafe {
            self.builder.add_instruction(
                &self.value,
                InstructionData {
                    kind: instruction,
                    location: None,
                    meta: None,
                },
                name.into(),
            )
        }
    }

    pub fn build(&mut self, instruction: Instr) -> Result<InstructionValue, ()> {
        unsafe {
            let name = instruction.default_name().to_owned();
            self.builder.add_instruction(
                &self.value,
                InstructionData {
                    kind: instruction,
                    location: None,
                    meta: None,
                },
                name,
            )
        }
    }

    pub fn set_instr_location(&self, instruction: InstructionValue, location: DebugLocationValue) {
        unsafe {
            self.builder
                .add_instruction_location(&instruction, location);
        }
    }

    pub fn set_instr_metadata(&self, instruction: InstructionValue, location: DebugMetadataValue) {
        unsafe {
            self.builder
                .add_instruction_metadata(&instruction, location);
        }
    }

    pub fn terminate(&mut self, instruction: TerminatorKind) -> Result<(), ()> {
        unsafe { self.builder.terminate(&self.value, instruction) }
    }

    pub fn set_terminator_location(&mut self, location: DebugLocationValue) -> Result<(), ()> {
        unsafe { self.builder.set_terminator_location(&self.value, location) }
    }

    /// Delete block if it is unused. Return true if deleted, false if not.
    pub fn delete_if_unused(&mut self) -> Result<bool, ()> {
        unsafe {
            if !self.builder.is_block_used(self.value()) {
                self.builder.delete_block(&self.value)?;
                Ok(true)
            } else {
                Ok(false)
            }
        }
    }

    pub fn value(&self) -> BlockValue {
        self.value
    }
}

#[derive(Clone)]
pub struct InstructionData {
    kind: Instr,
    location: Option<DebugLocationValue>,
    meta: Option<DebugMetadataValue>,
}

#[derive(Clone, Copy, Hash)]
pub enum CmpPredicate {
    LT,
    LE,
    GT,
    GE,
    EQ,
    NE,
}

/// https://llvm.org/docs/LangRef.html#instruction-reference
#[derive(Clone)]
pub enum Instr {
    Param(usize),
    Constant(ConstValue),

    /// Add two integers
    Add(InstructionValue, InstructionValue),
    /// Add two floats
    FAdd(InstructionValue, InstructionValue),
    /// Subtract two integers
    Sub(InstructionValue, InstructionValue),
    /// Subtract two floats
    FSub(InstructionValue, InstructionValue),
    /// Multiply two integers
    Mul(InstructionValue, InstructionValue),
    /// Multiply two floats
    FMul(InstructionValue, InstructionValue),
    /// Divide two unsigned integers
    UDiv(InstructionValue, InstructionValue),
    /// Divide two signed integers
    SDiv(InstructionValue, InstructionValue),
    /// Divide two floats
    FDiv(InstructionValue, InstructionValue),
    /// Get the remainder from two unsigned integers
    URem(InstructionValue, InstructionValue),
    /// Get the remainder from two signed integers
    SRem(InstructionValue, InstructionValue),
    /// Get the remainder from two floats
    FRem(InstructionValue, InstructionValue),
    And(InstructionValue, InstructionValue),
    Phi(Vec<InstructionValue>),

    Alloca(Type),
    Load(InstructionValue, Type),
    Store(InstructionValue, InstructionValue),
    ArrayAlloca(Type, u32),
    GetElemPtr(InstructionValue, Vec<InstructionValue>),
    GetStructElemPtr(InstructionValue, u32),
    ExtractValue(InstructionValue, u32),

    /// Integer Comparison
    ICmp(CmpPredicate, InstructionValue, InstructionValue),
    /// FLoat Comparison
    FCmp(CmpPredicate, InstructionValue, InstructionValue),

    /// The `trunc` instruction truncates the high order bits in value and
    /// converts the remaining bits to ty2. Since the source size must be larger
    /// than the destination size, `trunc` cannot be a no-op cast. It will
    /// always truncate bits.
    Trunc(InstructionValue, Type),
    /// The `zext` fills the high order bits of the value with zero bits until
    /// it reaches the size of the destination type, ty2.
    ZExt(InstructionValue, Type),
    /// The `sext` instruction performs a sign extension by copying the sign bit
    /// (highest order bit) of the value until it reaches the bit size of the
    /// type ty2.
    SExt(InstructionValue, Type),
    /// The `fptrunc` instruction casts a value from a larger floating-point
    /// type to a smaller floating-point type.
    FPTrunc(InstructionValue, Type),
    /// The `fpext` instruction extends the value from a smaller floating-point
    /// type to a larger floating-point type.
    FPExt(InstructionValue, Type),
    /// The `fptoui` instruction takes a value to cast, which must be a scalar
    /// or vector floating-point value, and a type to cast it to ty2, which must
    /// be an integer type.
    FPToUI(InstructionValue, Type),
    /// The `fptosi` instruction takes a value to cast, which must be a scalar
    /// or vector floating-point value, and a type to cast it to ty2, which must
    /// be an integer type.
    FPToSI(InstructionValue, Type),
    /// The `uitofp` instruction takes a value to cast, which must be a scalar
    /// or vector integer value, and a type to cast it to ty2, which must be an
    /// floating-point type.
    UIToFP(InstructionValue, Type),
    /// The `sitofp` instruction takes a value to cast, which must be a scalar
    /// or vector integer value, and a type to cast it to ty2, which must be an
    /// floating-point type
    SIToFP(InstructionValue, Type),
    /// The `ptrtoint` instruction converts value to integer type ty2 by
    /// interpreting the all pointer representation bits as an integer
    /// (equivalent to a bitcast) and either truncating or zero extending that
    /// value to the size of the integer type.
    PtrToInt(InstructionValue, Type),
    /// The `inttoptr` instruction converts value to type ty2 by applying either
    /// a zero extension or a truncation depending on the size of the integer
    /// value.
    IntToPtr(InstructionValue, Type),
    /// The `bitcast` instruction converts value to type ty2. It is always a
    /// no-op cast because no bits change with this conversion.
    BitCast(InstructionValue, Type),

    FunctionCall(FunctionValue, Vec<InstructionValue>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F16,
    F32B,
    F32,
    F64,
    F80,
    F128,
    F128PPC,
    Bool,
    Void,
    CustomType(TypeValue),
    Array(Box<Type>, u64),
    Ptr(Box<Type>),
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    Bool(bool),
    Str(String),
    F16(f32),
    F32B(f32),
    F32(f32),
    F64(f64),
    F80(f64),
    F128(f64),
    F128PPC(f64),
}

#[derive(Clone, Hash)]
pub enum TerminatorKind {
    Ret(InstructionValue),
    RetVoid,
    Br(BlockValue),
    CondBr(InstructionValue, BlockValue, BlockValue),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TypeData {
    name: String,
    kind: CustomTypeKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum CustomTypeKind {
    NamedStruct(NamedStruct),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct NamedStruct(pub String, pub Vec<Type>);

impl ConstValue {
    pub fn get_type(&self) -> Type {
        use Type::*;
        match self {
            ConstValue::I8(_) => I8,
            ConstValue::I16(_) => I16,
            ConstValue::I32(_) => I32,
            ConstValue::I64(_) => I64,
            ConstValue::I128(_) => I128,
            ConstValue::U8(_) => U8,
            ConstValue::U16(_) => U16,
            ConstValue::U32(_) => U32,
            ConstValue::U64(_) => U64,
            ConstValue::U128(_) => U128,
            ConstValue::Str(_) => Type::Ptr(Box::new(U8)),
            ConstValue::Bool(_) => Bool,
            ConstValue::F16(_) => F16,
            ConstValue::F32B(_) => F32B,
            ConstValue::F32(_) => F32,
            ConstValue::F64(_) => F64,
            ConstValue::F80(_) => F80,
            ConstValue::F128(_) => F128,
            ConstValue::F128PPC(_) => F128PPC,
        }
    }
}

impl Type {
    pub fn comparable(&self) -> bool {
        match self {
            Type::I8 => true,
            Type::I16 => true,
            Type::I32 => true,
            Type::I64 => true,
            Type::I128 => true,
            Type::U8 => true,
            Type::U16 => true,
            Type::U32 => true,
            Type::U64 => true,
            Type::U128 => true,
            Type::Bool => true,
            Type::Void => false,
            Type::Ptr(_) => false,
            Type::CustomType(_) => false,
            Type::Array(_, _) => false,
            Type::F16 => true,
            Type::F32B => true,
            Type::F32 => true,
            Type::F64 => true,
            Type::F80 => true,
            Type::F128 => true,
            Type::F128PPC => true,
        }
    }

    pub fn signed(&self) -> bool {
        match self {
            Type::I8 => true,
            Type::I16 => true,
            Type::I32 => true,
            Type::I64 => true,
            Type::I128 => true,
            Type::U8 => false,
            Type::U16 => false,
            Type::U32 => false,
            Type::U64 => false,
            Type::U128 => false,
            Type::Bool => false,
            Type::Void => false,
            Type::Ptr(_) => false,
            Type::CustomType(_) => false,
            Type::Array(_, _) => false,
            Type::F16 => true,
            Type::F32B => true,
            Type::F32 => true,
            Type::F64 => true,
            Type::F80 => true,
            Type::F128 => true,
            Type::F128PPC => true,
        }
    }

    pub fn category(&self) -> TypeCategory {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::I128
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::U128 => TypeCategory::Integer,
            Type::F16
            | Type::F32B
            | Type::F32
            | Type::F64
            | Type::F80
            | Type::F128
            | Type::F128PPC => TypeCategory::Real,
            Type::Bool | Type::Void | Type::CustomType(_) | Type::Array(_, _) | Type::Ptr(_) => {
                TypeCategory::Other
            }
        }
    }

    pub fn cast_instruction(&self, value: InstructionValue, other: &Type) -> Option<Instr> {
        use Type::*;
        match (self, other) {
            (I8, I16 | I32 | I64 | I128) => Some(Instr::SExt(value, other.clone())),
            (I16, I32 | I64 | I128) => Some(Instr::SExt(value, other.clone())),
            (I32, I64 | I128) => Some(Instr::SExt(value, other.clone())),
            (I64, I128) => Some(Instr::SExt(value, other.clone())),
            (I128 | U128, I64 | U64 | I32 | U32 | I16 | U16 | I8 | U8) => {
                Some(Instr::Trunc(value, other.clone()))
            }
            (I64 | U64, I32 | U32 | I16 | U16 | I8 | U8) => {
                Some(Instr::Trunc(value, other.clone()))
            }
            (I32 | U32, I16 | U16 | I8 | U8) => Some(Instr::Trunc(value, other.clone())),
            (I16 | U16, I8 | U8) => Some(Instr::Trunc(value, other.clone())),
            (U8 | I8, U8 | I8 | U16 | I16 | U32 | I32 | U64 | I64 | U128 | I128) => {
                Some(Instr::ZExt(value, other.clone()))
            }
            (U16 | I16, U16 | I16 | U32 | I32 | U64 | I64 | U128 | I128) => {
                Some(Instr::ZExt(value, other.clone()))
            }
            (U32 | I32, U32 | I32 | U64 | I64 | U128 | I128) => {
                Some(Instr::ZExt(value, other.clone()))
            }
            (U64 | I64, U64 | I64 | U128 | I128) => Some(Instr::ZExt(value, other.clone())),
            (U128 | I128, U128 | I128) => Some(Instr::ZExt(value, other.clone())),
            (U8 | U16 | U32 | U64 | U128, F16 | F32 | F32B | F64 | F80 | F128 | F128PPC) => {
                Some(Instr::UIToFP(value, other.clone()))
            }
            (I8 | I16 | I32 | I64 | I128, F16 | F32 | F32B | F64 | F80 | F128 | F128PPC) => {
                Some(Instr::SIToFP(value, other.clone()))
            }
            (F16 | F32 | F32B | F64 | F80 | F128 | F128PPC, U8 | U16 | U32 | U64 | U128) => {
                Some(Instr::FPToUI(value, other.clone()))
            }
            (F16 | F32 | F32B | F64 | F80 | F128 | F128PPC, I8 | I16 | I32 | I64 | I128) => {
                Some(Instr::FPToSI(value, other.clone()))
            }
            (I128 | U128 | I64 | U64 | I32 | U32 | I16 | U16 | I8 | U8, Ptr(_)) => {
                Some(Instr::IntToPtr(value, other.clone()))
            }
            (Ptr(_), I128 | U128 | I64 | U64 | I32 | U32 | I16 | U16 | I8 | U8) => {
                Some(Instr::PtrToInt(value, other.clone()))
            }
            (F16, F32 | F32B | F64 | F80 | F128 | F128PPC) => {
                Some(Instr::FPExt(value, other.clone()))
            }
            (F32 | F32B, F64 | F80 | F128 | F128PPC) => Some(Instr::FPExt(value, other.clone())),
            (F64, F80 | F128 | F128PPC) => Some(Instr::FPExt(value, other.clone())),
            (F80, F128 | F128PPC) => Some(Instr::FPExt(value, other.clone())),
            (F128PPC | F128, F80 | F64 | F32B | F32 | F16) => {
                Some(Instr::FPTrunc(value, other.clone()))
            }
            (F80, F64 | F32B | F32 | F16) => Some(Instr::FPTrunc(value, other.clone())),
            (F64, F32B | F32 | F16) => Some(Instr::FPTrunc(value, other.clone())),
            (F32B | F32, F16) => Some(Instr::FPTrunc(value, other.clone())),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeCategory {
    Integer,
    Real,
    Other,
}

impl TerminatorKind {
    pub(crate) fn get_type(&self, builder: &Builder) -> Result<Type, ()> {
        use TerminatorKind::*;
        match self {
            Ret(instr_val) => instr_val.get_type(builder),
            RetVoid => Ok(Type::Void),
            Br(_) => Ok(Type::Void),
            CondBr(_, _, _) => Ok(Type::Void),
        }
    }
}
