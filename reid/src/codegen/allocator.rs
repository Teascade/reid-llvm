use std::collections::HashMap;

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    Block, Instr,
};

use mir::{CustomTypeKey, FunctionCall, FunctionDefinitionKind, IfExpression, TypeKind, WhileStatement};

use crate::mir::{self, FunctionParam, Metadata, SourceModuleId};

#[derive(Debug)]
pub struct Allocator {
    allocations: Vec<Allocation>,
}

pub struct AllocatorScope<'ctx, 'a> {
    pub(super) block: &'a mut Block<'ctx>,
    pub(super) mod_id: SourceModuleId,
    pub(super) type_values: &'a HashMap<CustomTypeKey, TypeValue>,
}

impl Allocator {
    pub fn from(func: &FunctionDefinitionKind, params: &Vec<FunctionParam>, scope: &mut AllocatorScope) -> Allocator {
        func.allocate(scope, params)
    }

    pub fn allocate(&mut self, meta: &Metadata, ty: &TypeKind) -> Option<InstructionValue> {
        let mut allocs = self.allocations.iter().cloned().enumerate();
        let val = allocs.find(|a| a.1 .0 == *meta && a.1 .1 == *ty);
        if let Some((i, _)) = val {
            self.allocations.remove(i);
        }
        val.map(|v| v.1 .2)
    }
}

#[derive(Clone, Debug)]
pub struct Allocation(Metadata, TypeKind, InstructionValue);

impl mir::FunctionDefinitionKind {
    fn allocate<'ctx, 'a>(
        &self,
        scope: &mut AllocatorScope<'ctx, 'a>,
        parameters: &Vec<mir::FunctionParam>,
    ) -> Allocator {
        let mut allocated = Vec::new();
        match &self {
            mir::FunctionDefinitionKind::Local(block, _) => {
                for param in parameters {
                    let allocation = scope
                        .block
                        .build_named(
                            param.name.clone(),
                            reid_lib::Instr::Alloca(param.ty.get_type(scope.type_values)),
                        )
                        .unwrap();
                    allocated.push(Allocation(param.meta, param.ty.clone(), allocation));
                }
                allocated.extend(block.allocate(scope));
            }
            mir::FunctionDefinitionKind::Extern(_) => {}
            mir::FunctionDefinitionKind::Intrinsic(_) => {}
        }

        Allocator { allocations: allocated }
    }
}

impl mir::Block {
    fn allocate<'ctx, 'a>(&self, scope: &mut AllocatorScope<'ctx, 'a>) -> Vec<Allocation> {
        let mut allocated = Vec::new();

        for statement in &self.statements {
            allocated.extend(statement.allocate(scope));
        }

        if let Some((_, ret_expr)) = &self.return_expression {
            if let Some(ret_expr) = ret_expr {
                allocated.extend(ret_expr.allocate(scope));
            }
        }

        allocated
    }
}

impl mir::Statement {
    fn allocate<'ctx, 'a>(&self, scope: &mut AllocatorScope<'ctx, 'a>) -> Vec<Allocation> {
        let mut allocated = Vec::new();

        match &self.0 {
            mir::StmtKind::Let(named_variable_ref, _, expression) => {
                allocated.extend(expression.allocate(scope));
                let allocation = scope
                    .block
                    .build_named(
                        named_variable_ref.1.clone(),
                        reid_lib::Instr::Alloca(named_variable_ref.0.get_type(scope.type_values)),
                    )
                    .unwrap();
                allocated.push(Allocation(
                    named_variable_ref.2,
                    named_variable_ref.0.clone(),
                    allocation,
                ));
            }
            mir::StmtKind::Set(lhs, rhs) => {
                allocated.extend(lhs.allocate(scope));
                allocated.extend(rhs.allocate(scope));
            }
            mir::StmtKind::Import(_) => {}
            mir::StmtKind::Expression(expression) => {
                allocated.extend(expression.allocate(scope));
            }
            mir::StmtKind::While(WhileStatement { condition, block, .. }) => {
                allocated.extend(condition.allocate(scope));
                allocated.extend(block.allocate(scope));
            }
        }

        allocated
    }
}

impl mir::Expression {
    fn allocate<'ctx, 'a>(&self, scope: &mut AllocatorScope<'ctx, 'a>) -> Vec<Allocation> {
        let mut allocated = Vec::new();

        match &self.0 {
            mir::ExprKind::Variable(_) => {}
            mir::ExprKind::Indexed(expr, _, idx) => {
                allocated.extend(expr.allocate(scope));
                allocated.extend(idx.allocate(scope));
            }
            mir::ExprKind::Accessed(expression, ..) => {
                allocated.extend(expression.allocate(scope));
            }
            mir::ExprKind::Array(expressions) => {
                let (_, ty) = self.return_type(&Default::default(), scope.mod_id).unwrap();
                let TypeKind::Array(elem_ty, _) = &ty else { panic!() };
                let array_name = format!("{}.{}", elem_ty, expressions.len());

                let allocation = scope
                    .block
                    .build_named(array_name, Instr::Alloca(ty.get_type(scope.type_values)))
                    .unwrap();
                allocated.push(Allocation(self.1, ty, allocation));

                for expression in expressions {
                    allocated.extend(expression.allocate(scope));
                }
            }
            mir::ExprKind::Struct(key, items) => {
                let (_, ty) = self.return_type(&Default::default(), scope.mod_id).unwrap();
                let allocation = scope
                    .block
                    .build_named(key.0.clone(), Instr::Alloca(ty.get_type(scope.type_values)))
                    .unwrap();
                allocated.push(Allocation(self.1, ty, allocation));

                for (field_name, expression, _) in items {
                    allocated.extend(expression.allocate(scope));

                    let (_, ty) = expression.return_type(&Default::default(), scope.mod_id).unwrap();

                    let allocation = scope
                        .block
                        .build_named(field_name, Instr::Alloca(ty.get_type(scope.type_values)))
                        .unwrap();
                    allocated.push(Allocation(expression.1, ty, allocation));
                }
            }
            mir::ExprKind::Literal(_) => {}
            mir::ExprKind::BinOp(_, lhs, rhs, _) => {
                allocated.extend(lhs.allocate(scope));
                allocated.extend(rhs.allocate(scope));
            }
            mir::ExprKind::FunctionCall(fn_call) => allocated.extend(fn_call.allocate(&fn_call.name, scope)),
            mir::ExprKind::If(IfExpression(cond, then_ex, else_ex)) => {
                allocated.extend(cond.allocate(scope));
                allocated.extend(then_ex.allocate(scope));
                if let Some(else_ex) = else_ex.as_ref() {
                    allocated.extend(else_ex.allocate(scope));
                }
            }
            mir::ExprKind::Block(block) => {
                allocated.extend(block.allocate(scope));
            }
            mir::ExprKind::Borrow(_, _) => {}
            mir::ExprKind::Deref(_) => {}
            mir::ExprKind::CastTo(expression, _) => {
                allocated.extend(expression.allocate(scope));
            }
            mir::ExprKind::AssociatedFunctionCall(ty, fn_call) => {
                allocated.extend(fn_call.allocate(&format!("{}::{}", ty, fn_call.name), scope))
            }
            mir::ExprKind::GlobalRef(..) => {}
        }

        allocated
    }
}

impl mir::FunctionCall {
    fn allocate<'ctx, 'a>(&self, name: &String, scope: &mut AllocatorScope<'ctx, 'a>) -> Vec<Allocation> {
        let mut allocated = Vec::new();

        for param in &self.parameters {
            allocated.extend(param.allocate(scope));
        }

        if self.return_type != TypeKind::Void {
            let allocation = scope
                .block
                .build_named(name, Instr::Alloca(self.return_type.get_type(scope.type_values)))
                .unwrap();
            allocated.push(Allocation(self.meta, self.return_type.clone(), allocation));
        }

        allocated
    }
}
