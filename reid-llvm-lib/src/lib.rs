//! Reid LLVM Lib is an ergonomic Rust'y API which is used to produce a
//! Low-Level IR (LLIR) using [`Context`] and [`Builder`]. This Builder can then
//! be used at the end to compile said LLIR into LLVM IR.

use std::{fmt::Debug, marker::PhantomData};

use builder::{BlockValue, Builder, FunctionValue, InstructionValue, ModuleValue, TypeValue};
use debug::PrintableModule;
use util::match_types;

pub mod builder;
pub mod compile;
mod debug;
mod util;

// pub struct InstructionValue(BlockValue, usize);

#[derive(Debug)]
pub struct Context {
    builder: Builder,
}

impl Context {
    pub fn new() -> Context {
        Context {
            builder: Builder::new(),
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
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct ModuleData {
    name: String,
    is_main: bool,
}

pub struct Module<'ctx> {
    phantom: PhantomData<&'ctx ()>,
    builder: Builder,
    value: ModuleValue,
}

impl<'ctx> Module<'ctx> {
    pub fn function(
        &mut self,
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
                    },
                ),
            }
        }
    }

    pub fn custom_type(&mut self, ty: CustomTypeKind) -> TypeValue {
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
}

#[derive(Debug, Clone, Hash)]
pub struct FunctionData {
    name: String,
    ret: Type,
    params: Vec<Type>,
    flags: FunctionFlags,
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
            is_pub: false,
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
                        deleted: false,
                    },
                ),
            }
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
    deleted: bool,
}

pub struct Block<'builder> {
    phantom: PhantomData<&'builder ()>,
    builder: Builder,
    value: BlockValue,
}

impl<'builder> Block<'builder> {
    pub fn build(&mut self, instruction: Instr) -> Result<InstructionValue, ()> {
        unsafe {
            self.builder
                .add_instruction(&self.value, InstructionData { kind: instruction })
        }
    }

    pub fn terminate(&mut self, instruction: TerminatorKind) -> Result<(), ()> {
        unsafe { self.builder.terminate(&self.value, instruction) }
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

#[derive(Clone, Hash)]
pub struct InstructionData {
    kind: Instr,
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

#[derive(Clone, Hash)]
pub enum Instr {
    Param(usize),
    Constant(ConstValue),

    Add(InstructionValue, InstructionValue),
    Sub(InstructionValue, InstructionValue),
    Mult(InstructionValue, InstructionValue),
    And(InstructionValue, InstructionValue),
    Phi(Vec<InstructionValue>),

    Alloca(String, Type),
    Load(InstructionValue, Type),
    Store(InstructionValue, InstructionValue),
    ArrayAlloca(Type, u32),
    GetElemPtr(InstructionValue, Vec<u32>),
    GetStructElemPtr(InstructionValue, u32),

    /// Integer Comparison
    ICmp(CmpPredicate, InstructionValue, InstructionValue),

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
    Bool,
    Void,
    CustomType(TypeValue),
    Ptr(Box<Type>),
}

#[derive(Debug, Clone, Hash)]
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
    StringPtr(String),
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

impl InstructionValue {
    pub(crate) fn get_type(&self, builder: &Builder) -> Result<Type, ()> {
        use Instr::*;
        unsafe {
            match &builder.instr_data(self).kind {
                Param(nth) => builder
                    .function_data(&self.0.0)
                    .params
                    .get(*nth)
                    .cloned()
                    .ok_or(()),
                Constant(c) => Ok(c.get_type()),
                Add(lhs, rhs) => match_types(lhs, rhs, &builder),
                Sub(lhs, rhs) => match_types(lhs, rhs, &builder),
                Mult(lhs, rhs) => match_types(lhs, rhs, &builder),
                And(lhs, rhs) => match_types(lhs, rhs, &builder),
                ICmp(_, _, _) => Ok(Type::Bool),
                FunctionCall(function_value, _) => Ok(builder.function_data(function_value).ret),
                Phi(values) => values.first().ok_or(()).and_then(|v| v.get_type(&builder)),
                Alloca(_, ty) => Ok(Type::Ptr(Box::new(ty.clone()))),
                Load(_, ty) => Ok(ty.clone()),
                Store(_, value) => value.get_type(builder),
                ArrayAlloca(ty, _) => Ok(Type::Ptr(Box::new(ty.clone()))),
                GetElemPtr(ptr, _) => ptr.get_type(builder),
                GetStructElemPtr(instr, idx) => {
                    let instr_ty = instr.get_type(builder)?;
                    dbg!(&builder);
                    dbg!(&instr, &instr_ty, idx);
                    let Type::Ptr(inner_ty) = instr_ty else {
                        panic!("GetStructElemPtr on non-pointer! ({:?})", &instr_ty)
                    };
                    let Type::CustomType(ty_value) = *inner_ty else {
                        panic!("GetStructElemPtr on non-struct! ({:?})", &inner_ty)
                    };
                    let field_ty = match builder.type_data(&ty_value).kind {
                        CustomTypeKind::NamedStruct(NamedStruct(_, fields)) => {
                            fields.get_unchecked(*idx as usize).clone()
                        }
                    };
                    Ok(Type::Ptr(Box::new(field_ty)))
                }
            }
        }
    }
}

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
            ConstValue::StringPtr(_) => Ptr(Box::new(I8)),
            ConstValue::Bool(_) => Bool,
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
        }
    }
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
