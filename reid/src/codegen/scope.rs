use std::{cell::RefCell, collections::HashMap, mem, rc::Rc};

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    debug_information::{DebugInformation, DebugProgramValue, DebugTypeValue},
    Block, Context, Function, Instr, Module,
};

use crate::{
    lexer::FullToken,
    mir::{pass::BinopKey, CustomTypeKey, SourceModuleId, TypeDefinition, TypeKind},
};

use super::{allocator::Allocator, ErrorKind, IntrinsicFunction, ModuleCodegen};

pub struct Scope<'ctx, 'scope> {
    pub(super) context: &'ctx Context,
    pub(super) modules: &'scope HashMap<SourceModuleId, ModuleCodegen<'ctx>>,
    pub(super) tokens: &'ctx Vec<FullToken>,
    pub(super) module: &'ctx Module<'ctx>,
    pub(super) module_id: SourceModuleId,
    pub(super) function: &'ctx Function<'ctx>,
    pub(super) block: Block<'ctx>,
    pub(super) types: &'scope HashMap<TypeValue, TypeDefinition>,
    pub(super) type_values: &'scope HashMap<CustomTypeKey, TypeValue>,
    pub(super) functions: &'scope HashMap<String, Function<'ctx>>,
    pub(super) binops: &'scope HashMap<BinopKey, StackBinopDefinition<'ctx>>,
    pub(super) stack_values: HashMap<String, StackValue>,
    pub(super) debug: Option<Debug<'ctx>>,
    pub(super) allocator: Rc<RefCell<Allocator>>,
}

impl<'ctx, 'a> Scope<'ctx, 'a> {
    pub fn with_block(&self, block: Block<'ctx>) -> Scope<'ctx, 'a> {
        Scope {
            block,
            modules: self.modules,
            tokens: self.tokens,
            function: self.function,
            context: self.context,
            module: self.module,
            module_id: self.module_id,
            functions: self.functions,
            types: self.types,
            type_values: self.type_values,
            stack_values: self.stack_values.clone(),
            debug: self.debug.clone(),
            allocator: self.allocator.clone(),
            binops: self.binops,
        }
    }

    /// Takes the block out from this scope, swaps the given block in it's place
    /// and returns the old block.
    pub fn swap_block(&mut self, block: Block<'ctx>) -> Block<'ctx> {
        let mut old_block = block;
        mem::swap(&mut self.block, &mut old_block);
        old_block
    }

    pub fn get_typedef(&self, key: &CustomTypeKey) -> Option<&TypeDefinition> {
        self.type_values.get(key).and_then(|v| self.types.get(v))
    }

    pub fn allocate(&self, name: &String, ty: &TypeKind) -> Option<InstructionValue> {
        self.allocator.borrow_mut().allocate(name, ty)
    }
}

#[derive(Debug, Clone)]
pub struct Debug<'ctx> {
    pub(super) info: &'ctx DebugInformation,
    pub(super) scope: DebugProgramValue,
    pub(super) types: &'ctx HashMap<TypeKind, DebugTypeValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackValue(pub(super) StackValueKind, pub(super) TypeKind);

impl StackValue {
    pub fn instr(&self) -> InstructionValue {
        self.0.instr()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackValueKind {
    Immutable(InstructionValue),
    Mutable(InstructionValue),
    Literal(InstructionValue),
}

impl StackValueKind {
    pub fn mutable(mutable: bool, instr: InstructionValue) -> StackValueKind {
        match mutable {
            true => StackValueKind::Mutable(instr),
            false => StackValueKind::Immutable(instr),
        }
    }

    pub fn instr(&self) -> InstructionValue {
        match &self {
            StackValueKind::Immutable(val) => *val,
            StackValueKind::Mutable(val) => *val,
            StackValueKind::Literal(val) => *val,
        }
    }

    pub fn derive(&self, instr: InstructionValue) -> StackValueKind {
        match &self {
            StackValueKind::Immutable(_) => StackValueKind::Immutable(instr),
            StackValueKind::Mutable(_) => StackValueKind::Mutable(instr),
            StackValueKind::Literal(_) => StackValueKind::Literal(instr),
        }
    }

    #[allow(dead_code)]
    fn map<F>(&self, lambda: F) -> StackValueKind
    where
        F: FnOnce(InstructionValue) -> InstructionValue,
    {
        self.derive(lambda(self.instr()))
    }
}

pub struct StackBinopDefinition<'ctx> {
    pub(super) parameters: ((String, TypeKind), (String, TypeKind)),
    pub(super) return_ty: TypeKind,
    pub(super) kind: StackBinopFunctionKind<'ctx>,
}

pub enum StackBinopFunctionKind<'ctx> {
    UserGenerated(Function<'ctx>),
    Intrinsic(&'ctx Box<dyn IntrinsicFunction>),
}

impl<'ctx> StackBinopDefinition<'ctx> {
    pub fn codegen<'a>(
        &self,
        lhs: &StackValue,
        rhs: &StackValue,
        scope: &mut Scope<'ctx, 'a>,
    ) -> Result<StackValue, ErrorKind> {
        let (lhs, rhs) = if lhs.1 == self.parameters.0 .1 && rhs.1 == self.parameters.1 .1 {
            (lhs, rhs)
        } else {
            (rhs, lhs)
        };
        match &self.kind {
            StackBinopFunctionKind::UserGenerated(ir) => {
                let instr = scope
                    .block
                    .build(Instr::FunctionCall(ir.value(), vec![lhs.instr(), rhs.instr()]))
                    .unwrap();
                Ok(StackValue(StackValueKind::Immutable(instr), self.return_ty.clone()))
            }
            StackBinopFunctionKind::Intrinsic(fun) => fun.codegen(scope, &[&lhs, &rhs]),
        }
    }
}
