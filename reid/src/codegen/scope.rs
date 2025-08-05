use std::{cell::RefCell, collections::HashMap, mem, rc::Rc};

use reid_lib::{
    builder::{FunctionValue, GlobalValue, InstructionValue, TypeValue},
    debug_information::{DebugInformation, DebugLocation, DebugScopeValue, DebugTypeValue},
    intrinsics::LLVMIntrinsic,
    Block, Context, Function, Instr, Module,
};

use crate::{
    codegen::intrinsics::LLVMIntrinsicKind,
    lexer::FullToken,
    mir::{
        self,
        pass::{AssociatedFunctionKey, BinopKey},
        CustomTypeKey, FunctionParam, Metadata, SourceModuleId, TypeDefinition, TypeKind,
    },
};

use super::{allocator::Allocator, ErrorKind, IntrinsicFunction};

pub struct Scope<'ctx, 'scope> {
    pub(super) context: &'ctx Context,
    pub(super) modules: &'scope HashMap<SourceModuleId, &'ctx mir::Module>,
    pub(super) tokens: &'ctx Vec<FullToken>,
    pub(super) module: &'ctx Module<'ctx>,
    pub(super) module_id: SourceModuleId,
    pub(super) function: &'ctx Function<'ctx>,
    pub(super) block: Block<'ctx>,
    pub(super) types: &'scope HashMap<TypeValue, TypeDefinition>,
    pub(super) type_values: &'scope HashMap<CustomTypeKey, TypeValue>,
    pub(super) type_map: &'scope HashMap<CustomTypeKey, TypeDefinition>,
    pub(super) assoc_functions: &'scope HashMap<AssociatedFunctionKey, ScopeFunctionKind<'ctx>>,
    pub(super) functions: &'scope HashMap<String, ScopeFunctionKind<'ctx>>,
    pub(super) binops: &'scope HashMap<BinopKey, StackBinopDefinition<'ctx>>,
    pub(super) stack_values: HashMap<String, StackValue>,
    pub(super) globals: &'scope HashMap<String, GlobalValue>,
    pub(super) debug: Option<Debug<'ctx>>,
    pub(super) allocator: Rc<RefCell<Allocator>>,
    pub(super) llvm_intrinsics: Rc<RefCell<HashMap<LLVMIntrinsicKind, FunctionValue>>>,
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
            type_map: self.type_map,
            stack_values: self.stack_values.clone(),
            debug: self.debug.clone(),
            allocator: self.allocator.clone(),
            globals: self.globals,
            binops: self.binops,
            llvm_intrinsics: self.llvm_intrinsics.clone(),
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

    pub fn allocate(&self, meta: &Metadata, ty: &TypeKind) -> Option<InstructionValue> {
        self.allocator.borrow_mut().allocate(meta, ty)
    }

    pub fn get_intrinsic(&self, kind: LLVMIntrinsicKind) -> FunctionValue {
        let mut intrinsics = self.llvm_intrinsics.borrow_mut();
        if let Some(fun) = intrinsics.get(&kind) {
            *fun
        } else {
            let intrinsic = self
                .module
                .intrinsic(match &kind {
                    LLVMIntrinsicKind::Max(ty) => LLVMIntrinsic::Max(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Min(ty) => LLVMIntrinsic::Min(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Abs(ty) => LLVMIntrinsic::Abs(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Memcpy(ty) => LLVMIntrinsic::Memcpy(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Sqrt(ty) => LLVMIntrinsic::Sqrt(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::PowI(lhs, rhs) => {
                        LLVMIntrinsic::PowI(lhs.get_type(self.type_values), rhs.get_type(self.type_values))
                    }
                    LLVMIntrinsicKind::Pow(ty) => LLVMIntrinsic::Pow(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Sin(ty) => LLVMIntrinsic::Sin(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Cos(ty) => LLVMIntrinsic::Cos(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Tan(ty) => LLVMIntrinsic::Tan(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::ASin(ty) => LLVMIntrinsic::ASin(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::ACos(ty) => LLVMIntrinsic::ACos(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::ATan(ty) => LLVMIntrinsic::ATan(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::ATan2(ty) => LLVMIntrinsic::ATan2(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::SinH(ty) => LLVMIntrinsic::SinH(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::CosH(ty) => LLVMIntrinsic::CosH(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::TanH(ty) => LLVMIntrinsic::TanH(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Log(ty) => LLVMIntrinsic::Log(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Log2(ty) => LLVMIntrinsic::Log2(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Log10(ty) => LLVMIntrinsic::Log10(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Copysign(ty) => LLVMIntrinsic::Copysign(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Floor(ty) => LLVMIntrinsic::Floor(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Ceil(ty) => LLVMIntrinsic::Ceil(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Trunc(ty) => LLVMIntrinsic::Trunc(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::RoundEven(ty) => LLVMIntrinsic::RoundEven(ty.get_type(self.type_values)),
                    LLVMIntrinsicKind::Round(ty) => LLVMIntrinsic::Round(ty.get_type(self.type_values)),
                })
                .unwrap();
            intrinsics.insert(kind, intrinsic.clone());
            intrinsic
        }
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
    pub(super) parameters: (FunctionParam, FunctionParam),
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
        let (lhs, rhs) = if lhs.1 == self.parameters.0.ty && rhs.1 == self.parameters.1.ty {
            (lhs, rhs)
        } else {
            (rhs, lhs)
        };
        let name = format!(
            "binop.{}.{}.{}.call",
            self.parameters.0.ty, self.parameters.1.ty, self.return_ty
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
