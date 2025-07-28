use std::{cell::RefCell, collections::HashMap, mem, rc::Rc};

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    debug_information::{DebugInformation, DebugLocation, DebugScopeValue, DebugTypeValue},
    Block, Context, Function, Instr, Module,
};

use crate::{
    lexer::FullToken,
    mir::{
        pass::{AssociatedFunctionKey, BinopKey},
        CustomTypeKey, SourceModuleId, TypeDefinition, TypeKind,
    },
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
    pub(super) assoc_functions: &'scope HashMap<AssociatedFunctionKey, ScopeFunctionKind<'ctx>>,
    pub(super) functions: &'scope HashMap<String, ScopeFunctionKind<'ctx>>,
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
            assoc_functions: self.assoc_functions,
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
    pub(super) scope: DebugScopeValue,
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
    pub(super) kind: ScopeFunctionKind<'ctx>,
}

pub enum ScopeFunctionKind<'ctx> {
    UserGenerated(Function<'ctx>),
    Intrinsic(&'ctx Box<dyn IntrinsicFunction>),
    IntrinsicOwned(Box<dyn IntrinsicFunction>),
}

impl<'ctx> StackBinopDefinition<'ctx> {
    pub fn codegen<'a>(
        &self,
        lhs: StackValue,
        rhs: StackValue,
        scope: &mut Scope<'ctx, 'a>,
    ) -> Result<StackValue, ErrorKind> {
        let (lhs, rhs) = if lhs.1 == self.parameters.0 .1 && rhs.1 == self.parameters.1 .1 {
            (lhs, rhs)
        } else {
            (rhs, lhs)
        };
        let name = format!(
            "binop.{}.{}.{}.call",
            self.parameters.0 .1, self.parameters.1 .1, self.return_ty
        );
        self.kind.codegen(&name, &[lhs, rhs], &self.return_ty, None, scope)
    }
}

impl<'ctx> ScopeFunctionKind<'ctx> {
    pub fn codegen<'a>(
        &self,
        name: &str,
        params: &[StackValue],
        return_ty: &TypeKind,
        location: Option<DebugLocation>,
        scope: &mut Scope<'ctx, 'a>,
    ) -> Result<StackValue, ErrorKind> {
        match self {
            ScopeFunctionKind::UserGenerated(function) => {
                let val = scope
                    .block
                    .build_named(
                        name,
                        Instr::FunctionCall(function.value(), params.iter().map(|p| p.instr()).collect()),
                    )
                    .unwrap();

                if let Some(debug) = &scope.debug {
                    if let Some(location) = location {
                        let location_val = debug.info.location(&debug.scope, location);
                        val.with_location(&mut scope.block, location_val);
                    }
                }
                Ok(StackValue(StackValueKind::Immutable(val), return_ty.clone()))
            }
            ScopeFunctionKind::Intrinsic(fun) => fun.codegen(scope, params),
            ScopeFunctionKind::IntrinsicOwned(fun) => fun.codegen(scope, params),
        }
    }
}
