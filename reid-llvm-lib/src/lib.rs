use std::marker::PhantomData;

use builder::{BlockValue, Builder, FunctionValue, InstructionValue, ModuleValue};

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

    pub fn module<'ctx>(&'ctx self, name: &str) -> Module<'ctx> {
        let value = self.builder.add_module(ModuleData {
            name: name.to_owned(),
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
}

pub struct Module<'ctx> {
    phantom: PhantomData<&'ctx ()>,
    builder: Builder,
    value: ModuleValue,
}

impl<'ctx> Module<'ctx> {
    pub fn function(&mut self, name: &str, ret: Type, params: Vec<Type>) -> Function<'ctx> {
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
                    },
                ),
            }
        }
    }

    pub fn value(&self) -> ModuleValue {
        self.value
    }
}

#[derive(Debug, Clone, Hash)]
pub struct FunctionData {
    name: String,
    ret: Type,
    params: Vec<Type>,
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
}

pub struct Block<'builder> {
    phantom: PhantomData<&'builder ()>,
    builder: Builder,
    value: BlockValue,
}

impl<'builder> Block<'builder> {
    pub fn build(&mut self, instruction: InstructionKind) -> Result<InstructionValue, ()> {
        unsafe {
            self.builder
                .add_instruction(&self.value, InstructionData { kind: instruction })
        }
    }

    pub fn terminate(&mut self, instruction: TerminatorKind) -> Result<(), ()> {
        unsafe { self.builder.terminate(&self.value, instruction) }
    }

    pub fn value(&self) -> BlockValue {
        self.value
    }
}

#[derive(Clone, Hash)]
pub struct InstructionData {
    kind: InstructionKind,
}

#[derive(Clone, Copy, Hash)]
pub enum IntPredicate {
    LessThan,
    GreaterThan,
}

#[derive(Clone, Hash)]
pub enum InstructionKind {
    Param(usize),
    Constant(ConstValue),
    Add(InstructionValue, InstructionValue),
    Sub(InstructionValue, InstructionValue),
    Phi(Vec<InstructionValue>),

    /// Integer Comparison
    ICmp(IntPredicate, InstructionValue, InstructionValue),

    FunctionCall(FunctionValue, Vec<InstructionValue>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Type {
    I32,
    I16,
    U32,
    Bool,
    Void,
}

#[derive(Debug, Clone, Hash)]
pub enum ConstValue {
    I32(i32),
    I16(i16),
    U32(u32),
}

#[derive(Clone, Hash)]
pub enum TerminatorKind {
    Ret(InstructionValue),
    Branch(BlockValue),
    CondBr(InstructionValue, BlockValue, BlockValue),
}

fn test() {
    use ConstValue::*;
    use InstructionKind::*;

    let context = Context::new();

    let mut module = context.module("test");

    let mut main = module.function("main", Type::I32, Vec::new());
    let mut m_entry = main.block("entry");

    let mut fibonacci = module.function("fibonacci", Type::I32, vec![Type::I32]);

    let arg = m_entry.build(Constant(I32(5))).unwrap();
    m_entry
        .build(FunctionCall(fibonacci.value, vec![arg]))
        .unwrap();

    let mut f_entry = fibonacci.block("entry");

    let num_3 = f_entry.build(Constant(I32(3))).unwrap();
    let param_n = f_entry.build(Param(0)).unwrap();
    let cond = f_entry
        .build(ICmp(IntPredicate::LessThan, param_n, num_3))
        .unwrap();

    let mut then_b = fibonacci.block("then");
    let mut else_b = fibonacci.block("else");

    f_entry
        .terminate(TerminatorKind::CondBr(cond, then_b.value, else_b.value))
        .unwrap();

    let ret_const = then_b.build(Constant(I32(1))).unwrap();
    then_b.terminate(TerminatorKind::Ret(ret_const)).unwrap();

    let const_1 = else_b.build(Constant(I32(1))).unwrap();
    let const_2 = else_b.build(Constant(I32(2))).unwrap();
    let param_1 = else_b.build(Sub(param_n, const_1)).unwrap();
    let param_2 = else_b.build(Sub(param_n, const_2)).unwrap();
    let call_1 = else_b
        .build(FunctionCall(fibonacci.value, vec![param_1]))
        .unwrap();
    let call_2 = else_b
        .build(FunctionCall(fibonacci.value, vec![param_2]))
        .unwrap();

    let add = else_b.build(Add(call_1, call_2)).unwrap();

    else_b.terminate(TerminatorKind::Ret(add)).unwrap();

    dbg!(context);
}
