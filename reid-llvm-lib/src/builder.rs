//! This module contains simply [`Builder`] and it's related utility Values.
//! Builder is the actual struct being modified when building the LLIR.

use std::{cell::RefCell, rc::Rc};

use crate::{
    Block, BlockData, CustomTypeKind, FunctionData, Instr, InstructionData, ModuleData,
    NamedStruct, TerminatorKind, Type, TypeCategory, TypeData,
    debug_information::{
        DebugInformation, DebugLocationValue, DebugMetadataValue, DebugProgramValue,
        InstructionDebugRecordData,
    },
    util::match_types,
};

#[derive(Clone, Hash, Copy, PartialEq, Eq)]
pub struct ModuleValue(pub(crate) usize);

#[derive(Clone, Hash, Copy, PartialEq, Eq)]
pub struct TypeValue(pub(crate) ModuleValue, pub(crate) usize);

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
    pub(crate) types: Vec<TypeHolder>,
    pub(crate) debug_information: Option<DebugInformation>,
}

#[derive(Clone)]
pub struct TypeHolder {
    pub(crate) value: TypeValue,
    pub(crate) data: TypeData,
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
    pub(crate) name: String,
    pub(crate) record: Option<InstructionDebugRecordData>,
}

#[derive(Clone)]
pub(crate) struct Builder {
    pub(crate) modules: Rc<RefCell<Vec<ModuleHolder>>>,
    pub(crate) producer: String,
}

impl Builder {
    pub fn new(producer: String) -> Builder {
        Builder {
            modules: Rc::new(RefCell::new(Vec::new())),
            producer,
        }
    }

    pub(crate) fn add_module(&self, data: ModuleData) -> ModuleValue {
        let value = ModuleValue(self.modules.borrow().len());
        self.modules.borrow_mut().push(ModuleHolder {
            value,
            data,
            functions: Vec::new(),
            types: Vec::new(),
            debug_information: None,
        });
        value
    }

    pub(crate) fn set_debug_information(
        &self,
        mod_val: &ModuleValue,
        debug_info: DebugInformation,
    ) {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(mod_val.0);
            module.debug_information = Some(debug_info);
        }
    }

    pub(crate) unsafe fn add_type(&self, mod_val: &ModuleValue, data: TypeData) -> TypeValue {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(mod_val.0);
            let value = TypeValue(module.value, module.types.len());
            module.types.push(TypeHolder { value, data });
            value
        }
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
        name: String,
    ) -> Result<InstructionValue, ()> {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(block_val.0.0.0);
            let function = module.functions.get_unchecked_mut(block_val.0.1);
            let block = function.blocks.get_unchecked_mut(block_val.1);
            let value = InstructionValue(block.value, block.instructions.len());
            block.instructions.push(InstructionHolder {
                value,
                data,
                name,
                record: None,
            });

            // Drop modules so that it is no longer mutable borrowed
            // (check_instruction requires an immutable borrow).
            drop(modules);

            self.check_instruction(&value)?;
            Ok(value)
        }
    }

    pub(crate) unsafe fn add_instruction_location(
        &self,
        value: &InstructionValue,
        location: DebugLocationValue,
    ) {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(value.0.0.0.0);
            let function = module.functions.get_unchecked_mut(value.0.0.1);
            let block = function.blocks.get_unchecked_mut(value.0.1);
            let instr = block.instructions.get_unchecked_mut(value.1);
            instr.data.location = Some(location)
        }
    }

    pub(crate) unsafe fn add_instruction_metadata(
        &self,
        value: &InstructionValue,
        metadata: DebugMetadataValue,
    ) {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(value.0.0.0.0);
            let function = module.functions.get_unchecked_mut(value.0.0.1);
            let block = function.blocks.get_unchecked_mut(value.0.1);
            let instr = block.instructions.get_unchecked_mut(value.1);
            instr.data.meta = Some(metadata)
        }
    }

    pub(crate) unsafe fn add_instruction_record(
        &self,
        value: &InstructionValue,
        record: InstructionDebugRecordData,
    ) {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(value.0.0.0.0);
            let function = module.functions.get_unchecked_mut(value.0.0.1);
            let block = function.blocks.get_unchecked_mut(value.0.1);
            let instr = block.instructions.get_unchecked_mut(value.1);
            instr.record = Some(record)
        }
    }

    pub(crate) unsafe fn set_debug_subprogram(
        &self,
        value: &FunctionValue,
        subprogram: DebugProgramValue,
    ) {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(value.0.0);
            let function = module.functions.get_unchecked_mut(value.1);
            function.data.debug = Some(subprogram)
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

    pub(crate) unsafe fn set_terminator_location(
        &self,
        block: &BlockValue,
        location: DebugLocationValue,
    ) -> Result<(), ()> {
        unsafe {
            let mut modules = self.modules.borrow_mut();
            let module = modules.get_unchecked_mut(block.0.0.0);
            let function = module.functions.get_unchecked_mut(block.0.1);
            let block = function.blocks.get_unchecked_mut(block.1);
            if let Some(_) = &block.data.terminator_location {
                Err(())
            } else {
                block.data.terminator_location = Some(location);
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

    pub(crate) unsafe fn type_data(&self, value: &TypeValue) -> TypeData {
        unsafe {
            self.modules
                .borrow()
                .get_unchecked(value.0.0)
                .types
                .get_unchecked(value.1)
                .data
                .clone()
        }
    }

    pub(crate) fn find_module<'ctx>(&'ctx self, value: ModuleValue) -> ModuleHolder {
        unsafe { self.modules.borrow().get_unchecked(value.0).clone() }
    }

    pub(crate) fn get_modules(&self) -> Rc<RefCell<Vec<ModuleHolder>>> {
        self.modules.clone()
    }

    pub fn check_instruction(&self, instruction: &InstructionValue) -> Result<(), ()> {
        unsafe {
            match self.instr_data(&instruction).kind {
                Instr::Param(_) => Ok(()),
                Instr::Constant(_) => Ok(()),
                Instr::Add(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::FAdd(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::Sub(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::FSub(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::Mul(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::FMul(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::UDiv(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::SDiv(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::FDiv(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::URem(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::SRem(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::FRem(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::And(lhs, rhs) => match_types(&lhs, &rhs, &self).map(|_| ()),
                Instr::ICmp(_, lhs, rhs) => {
                    let t = match_types(&lhs, &rhs, self)?;
                    if t.comparable() || t.category() != TypeCategory::Integer {
                        Ok(())
                    } else {
                        Err(()) // TODO error: Types not comparable
                    }
                }
                Instr::FCmp(_, lhs, rhs) => {
                    let t = match_types(&lhs, &rhs, self)?;
                    if t.comparable() || t.category() != TypeCategory::Real {
                        Ok(())
                    } else {
                        Err(()) // TODO error: Types not comparable
                    }
                }
                Instr::FunctionCall(fun, params) => {
                    let param_types = self.function_data(&fun).params;
                    dbg!(&params, &param_types);
                    if param_types.len() != params.len() {
                        return Err(()); // TODO error: invalid amount of params
                    }
                    for (a, b) in param_types.iter().zip(params) {
                        dbg!(b.get_type(&self)?);
                        if *a != b.get_type(&self)? {
                            return Err(()); // TODO error: params do not match
                        }
                    }
                    Ok(())
                }
                Instr::Phi(vals) => {
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
                Instr::Alloca(_) => Ok(()),
                Instr::Load(ptr, load_ty) => {
                    let ptr_ty = ptr.get_type(&self)?;
                    if let Type::Ptr(ptr_ty_inner) = ptr_ty {
                        if *ptr_ty_inner == load_ty {
                            Ok(())
                        } else {
                            Err(()) // TODO error: inner type mismatch
                        }
                    } else {
                        Err(()) // TODO error: not a pointer
                    }
                }
                Instr::Store(ptr, _) => {
                    let ty = ptr.get_type(&self)?;
                    if let Type::Ptr(_) = ty {
                        Ok(())
                    } else {
                        Err(()) // TODO error: not a pointer
                    }
                }
                Instr::ArrayAlloca(_, _) => Ok(()),
                Instr::GetElemPtr(ptr_val, _) => {
                    let ptr_ty = ptr_val.get_type(&self)?;
                    match ptr_ty {
                        Type::Ptr(_) => Ok(()),
                        _ => Err(()),
                    }
                }
                Instr::GetStructElemPtr(ptr_val, idx) => {
                    let ptr_ty = ptr_val.get_type(&self)?;
                    if let Type::Ptr(ty) = ptr_ty {
                        if let Type::CustomType(val) = *ty {
                            match self.type_data(&val).kind {
                                CustomTypeKind::NamedStruct(NamedStruct(_, fields)) => {
                                    if fields.len() <= idx as usize {
                                        return Err(()); // TODO error: no such field
                                    }
                                }
                            }
                            Ok(())
                        } else {
                            Err(()) // TODO error: not a struct
                        }
                    } else {
                        Err(()) // TODO error: not a pointer
                    }
                }
                Instr::ExtractValue(val, _) => {
                    let val_ty = val.get_type(&self)?;
                    match val_ty {
                        Type::CustomType(custom) => match self.type_data(&custom).kind {
                            CustomTypeKind::NamedStruct(_) => Ok(()),
                        },
                        Type::Array(_, _) => Ok(()),
                        _ => Err(()),
                    }
                }
                Instr::Trunc(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::ZExt(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::SExt(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::FPTrunc(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::FPExt(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::FPToUI(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::FPToSI(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::UIToFP(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::SIToFP(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::PtrToInt(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::IntToPtr(instr, ty) => instr.cast_to(self, &ty).map(|_| ()),
                Instr::BitCast(..) => Ok(()),
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
    pub fn with_location(self, block: &Block, location: DebugLocationValue) -> InstructionValue {
        unsafe {
            block.builder.add_instruction_location(&self, location);
        }
        self
    }

    pub fn maybe_location(
        self,
        block: &mut Block,
        location: Option<DebugLocationValue>,
    ) -> InstructionValue {
        unsafe {
            if let Some(location) = location {
                block.builder.add_instruction_location(&self, location);
            }
        }
        self
    }

    pub fn add_record(&self, block: &mut Block, record: InstructionDebugRecordData) {
        unsafe {
            block.builder.add_instruction_record(self, record);
        }
    }

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
                FAdd(lhs, rhs) => match_types(lhs, rhs, &builder),
                Sub(lhs, rhs) => match_types(lhs, rhs, &builder),
                FSub(lhs, rhs) => match_types(lhs, rhs, &builder),
                Mul(lhs, rhs) => match_types(lhs, rhs, &builder),
                FMul(lhs, rhs) => match_types(lhs, rhs, &builder),
                UDiv(lhs, rhs) => match_types(lhs, rhs, &builder),
                SDiv(lhs, rhs) => match_types(lhs, rhs, &builder),
                FDiv(lhs, rhs) => match_types(lhs, rhs, &builder),
                URem(lhs, rhs) => match_types(lhs, rhs, &builder),
                SRem(lhs, rhs) => match_types(lhs, rhs, &builder),
                FRem(lhs, rhs) => match_types(lhs, rhs, &builder),
                And(lhs, rhs) => match_types(lhs, rhs, &builder),
                ICmp(_, _, _) => Ok(Type::Bool),
                FCmp(_, _, _) => Ok(Type::Bool),
                FunctionCall(function_value, _) => Ok(builder.function_data(function_value).ret),
                Phi(values) => values.first().ok_or(()).and_then(|v| v.get_type(&builder)),
                Alloca(ty) => Ok(Type::Ptr(Box::new(ty.clone()))),
                Load(_, ty) => Ok(ty.clone()),
                Store(_, value) => value.get_type(builder),
                ArrayAlloca(ty, _) => Ok(Type::Ptr(Box::new(ty.clone()))),
                GetElemPtr(instr, _) => {
                    let instr_ty = instr.get_type(builder)?;
                    let Type::Ptr(inner_ty) = &instr_ty else {
                        panic!("GetStructElemPtr on non-pointer! ({:?})", &instr_ty)
                    };
                    match *inner_ty.clone() {
                        Type::Array(elem_ty, _) => Ok(Type::Ptr(Box::new(*elem_ty.clone()))),
                        _ => Ok(instr_ty),
                    }
                }
                GetStructElemPtr(instr, idx) => {
                    let instr_ty = instr.get_type(builder)?;
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
                ExtractValue(instr, idx) => {
                    let instr_ty = instr.get_type(builder)?;
                    Ok(match instr_ty {
                        Type::CustomType(struct_ty) => {
                            let data = builder.type_data(&struct_ty);
                            match data.kind {
                                CustomTypeKind::NamedStruct(named_struct) => {
                                    named_struct.1.get(*idx as usize).unwrap().clone()
                                }
                            }
                        }
                        Type::Array(elem_ty, _) => *elem_ty.clone(),
                        _ => return Err(()),
                    })
                }
                Trunc(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                ZExt(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                SExt(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                FPTrunc(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                FPExt(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                FPToUI(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                FPToSI(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                UIToFP(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                SIToFP(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                PtrToInt(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                IntToPtr(instr, ty) => instr.cast_to(builder, ty).map(|_| ty.clone()),
                BitCast(_, ty) => Ok(ty.clone()),
            }
        }
    }

    fn cast_to(&self, builder: &Builder, ty: &Type) -> Result<Instr, ()> {
        self.get_type(builder)?
            .cast_instruction(*self, &ty)
            .ok_or(())
    }
}
