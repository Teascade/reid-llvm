//! This module contains simply [`Builder`] and it's related utility Values.
//! Builder is the actual struct being modified when building the LLIR.

use std::{cell::RefCell, rc::Rc};

use crate::{
    BlockData, ConstValue, FunctionData, Instr, InstructionData, ModuleData, TerminatorKind, Type,
    util::match_types,
};

#[derive(Clone, Hash, Copy, PartialEq, Eq)]
pub struct ModuleValue(pub(crate) usize);

#[derive(Clone, Hash, Copy, PartialEq, Eq)]
pub struct FunctionValue(pub(crate) ModuleValue, pub(crate) usize);

#[derive(Clone, Hash, Copy, PartialEq, Eq)]
pub struct BlockValue(pub(crate) FunctionValue, pub(crate) usize);

#[derive(Clone, Hash, Copy, PartialEq, Eq)]
pub struct InstructionValue(pub(crate) BlockValue, pub(crate) usize);

#[derive(Clone)]
pub struct ModuleHolder {
    pub(crate) value: ModuleValue,
    pub(crate) data: ModuleData,
    pub(crate) functions: Vec<FunctionHolder>,
}

#[derive(Clone)]
pub struct FunctionHolder {
    pub(crate) value: FunctionValue,
    pub(crate) data: FunctionData,
    pub(crate) blocks: Vec<BlockHolder>,
}

#[derive(Clone)]
pub struct BlockHolder {
    pub(crate) value: BlockValue,
    pub(crate) data: BlockData,
    pub(crate) instructions: Vec<InstructionHolder>,
}

#[derive(Clone)]
pub struct InstructionHolder {
    pub(crate) value: InstructionValue,
    pub(crate) data: InstructionData,
}

#[derive(Clone)]
pub(crate) struct Builder {
    modules: Rc<RefCell<Vec<ModuleHolder>>>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            modules: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub(crate) fn add_module(&self, data: ModuleData) -> ModuleValue {
        let value = ModuleValue(self.modules.borrow().len());
        self.modules.borrow_mut().push(ModuleHolder {
            value,
            data,
            functions: Vec::new(),
        });
        value
    }

    pub(crate) unsafe fn add_function(
        &self,
        mod_val: &ModuleValue,
        data: FunctionData,
    ) -> FunctionValue {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(mod_val.0);
            let value = FunctionValue(module.value, module.functions.len());
            module.functions.push(FunctionHolder {
                value,
                data,
                blocks: Vec::new(),
            });
            value
        }
    }

    pub(crate) unsafe fn add_block(&self, fun_val: &FunctionValue, data: BlockData) -> BlockValue {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(fun_val.0.0);
            let function = module.functions.get_unchecked_mut(fun_val.1);
            let value = BlockValue(function.value, function.blocks.len());
            function.blocks.push(BlockHolder {
                value,
                data,
                instructions: Vec::new(),
            });
            value
        }
    }

    pub(crate) unsafe fn add_instruction(
        &self,
        block_val: &BlockValue,
        data: InstructionData,
    ) -> Result<InstructionValue, ()> {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(block_val.0.0.0);
            let function = module.functions.get_unchecked_mut(block_val.0.1);
            let block = function.blocks.get_unchecked_mut(block_val.1);
            let value = InstructionValue(block.value, block.instructions.len());
            block.instructions.push(InstructionHolder { value, data });

            // Drop modules so that it is no longer mutable borrowed
            // (check_instruction requires an immutable borrow).
            drop(modules);

            self.check_instruction(&value)?;
            Ok(value)
        }
    }

    pub(crate) unsafe fn terminate(
        &self,
        block: &BlockValue,
        value: TerminatorKind,
    ) -> Result<(), ()> {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(block.0.0.0);
            let function = module.functions.get_unchecked_mut(block.0.1);
            let block = function.blocks.get_unchecked_mut(block.1);
            if let Some(_) = &block.data.terminator {
                Err(())
            } else {
                block.data.terminator = Some(value);
                Ok(())
            }
        }
    }

    pub(crate) unsafe fn delete_block(&self, block: &BlockValue) -> Result<(), ()> {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(block.0.0.0);
            let function = module.functions.get_unchecked_mut(block.0.1);
            let block = function.blocks.get_unchecked_mut(block.1);
            block.data.deleted = true;
            Ok(())
        }
    }

    #[allow(dead_code)]
    pub(crate) unsafe fn module_data(&self, value: &ModuleValue) -> ModuleData {
        unsafe { self.modules.borrow().get_unchecked(value.0).data.clone() }
    }

    pub(crate) unsafe fn function_data(&self, value: &FunctionValue) -> FunctionData {
        unsafe {
            self.modules
                .borrow()
                .get_unchecked(value.0.0)
                .functions
                .get_unchecked(value.1)
                .data
                .clone()
        }
    }

    #[allow(dead_code)]
    pub(crate) unsafe fn block_data(&self, value: &BlockValue) -> BlockData {
        unsafe {
            self.modules
                .borrow()
                .get_unchecked(value.0.0.0)
                .functions
                .get_unchecked(value.0.1)
                .blocks
                .get_unchecked(value.1)
                .data
                .clone()
        }
    }

    pub(crate) unsafe fn instr_data(&self, value: &InstructionValue) -> InstructionData {
        unsafe {
            self.modules
                .borrow()
                .get_unchecked(value.0.0.0.0)
                .functions
                .get_unchecked(value.0.0.1)
                .blocks
                .get_unchecked(value.0.1)
                .instructions
                .get_unchecked(value.1)
                .data
                .clone()
        }
    }

    pub(crate) fn get_modules(&self) -> Rc<RefCell<Vec<ModuleHolder>>> {
        self.modules.clone()
    }

    pub fn check_instruction(&self, instruction: &InstructionValue) -> Result<(), ()> {
        use super::Instr::*;
        unsafe {
            match self.instr_data(&instruction).kind {
                Param(_) => Ok(()),
                Constant(_) => Ok(()),
                Add(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Sub(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Mult(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                And(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                ICmp(_, lhs, rhs) => {
                    let t = match_types(&lhs, &rhs, self)?;
                    if t.comparable() {
                        Ok(())
                    } else {
                        Err(()) // TODO error: Types not comparable
                    }
                }
                FunctionCall(fun, params) => {
                    let param_types = self.function_data(&fun).params;
                    if param_types.len() != params.len() {
                        return Err(()); // TODO error: invalid amount of params
                    }
                    for (a, b) in param_types.iter().zip(params) {
                        if *a != b.get_type(&self)? {
                            return Err(()); // TODO error: params do not match
                        }
                    }
                    Ok(())
                }
                Phi(vals) => {
                    let mut iter = vals.iter();
                    // TODO error: Phi must contain at least one item

                    // TODO error: compile can actually crash here if any of the
                    // incoming values come from blocks that are added later
                    // than the one where this one exists.

                    let first = iter.next().ok_or(())?;
                    for item in iter {
                        match_types(first, item, &self)?;
                    }
                    Ok(())
                }
                Alloca(_, _) => Ok(()),
                Load(ptr, load_ty) => {
                    if let Ok(ptr_ty) = ptr.get_type(&self) {
                        if let Type::Ptr(ptr_ty_inner) = ptr_ty {
                            if *ptr_ty_inner == load_ty {
                                Ok(())
                            } else {
                                Err(())
                            }
                        } else {
                            Err(())
                        }
                    } else {
                        Err(())
                    }
                }
                Store(ptr, _) => {
                    if let Ok(ty) = ptr.get_type(&self) {
                        if let Type::Ptr(_) = ty {
                            Ok(())
                        } else {
                            Err(())
                        }
                    } else {
                        Err(())
                    }
                }
            }
        }
    }

    pub fn is_block_used(&self, block_v: BlockValue) -> bool {
        unsafe {
            let modules = self.modules.borrow();
            let module = modules.get_unchecked(block_v.0.0.0);
            let function = module.functions.get_unchecked(block_v.0.1);
            let block = function.blocks.get_unchecked(block_v.1);

            if block.instructions.len() > 0 || block.data.terminator.is_some() {
                return true;
            }

            for other in &function.blocks {
                if let Some(term) = &other.data.terminator {
                    match term {
                        TerminatorKind::Ret(_) => {}
                        TerminatorKind::RetVoid => {}
                        TerminatorKind::Br(other_val) => {
                            if other_val == &block_v {
                                return true;
                            }
                        }
                        TerminatorKind::CondBr(_, then_other_v, else_other_v) => {
                            if then_other_v == &block_v || else_other_v == &block_v {
                                return true;
                            }
                        }
                    }
                }
            }

            false
        }
    }
}

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
