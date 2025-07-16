//! Reid LLVM Lib is an ergonomic Rust'y API which is used to produce a
//! Low-Level IR (LLIR) using [`Context`] and [`Builder`]. This Builder can then
//! be used at the end to compile said LLIR into LLVM IR.

use std::{fmt::Debug, marker::PhantomData};

use builder::{BlockValue, Builder, FunctionValue, InstructionValue, ModuleValue, TypeValue};
use debug::PrintableModule;

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
