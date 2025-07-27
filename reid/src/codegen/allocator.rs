use std::collections::HashMap;

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    Block,
};

use mir::{CustomTypeKey, FunctionCall, FunctionDefinitionKind, IfExpression, TypeKind, WhileStatement};

use crate::mir;

#[derive(Debug)]
pub struct Allocator {
    allocations: Vec<Allocation>,
}

pub struct AllocatorScope<'ctx, 'a> {
    pub(super) block: &'a mut Block<'ctx>,
    pub(super) type_values: &'a HashMap<CustomTypeKey, TypeValue>,
}

impl Allocator {
    pub fn from(
        func: &FunctionDefinitionKind,
        params: &Vec<(String, TypeKind)>,
        scope: &mut AllocatorScope,
    ) -> Allocator {
        func.allocate(scope, params)
    }

    pub fn allocate(&mut self, name: &String, ty: &TypeKind) -> Option<InstructionValue> {
        let mut allocs = self.allocations.iter().cloned().enumerate();
        let val = allocs.find(|a| a.1 .0 == *name && a.1 .1 == *ty);
        if let Some((i, _)) = val {
            self.allocations.remove(i);
        }
        val.map(|v| v.1 .2)
    }
}

#[derive(Clone, Debug)]
pub struct Allocation(String, TypeKind, InstructionValue);

impl mir::FunctionDefinitionKind {
    fn allocate<'ctx, 'a>(
        &self,
        scope: &mut AllocatorScope<'ctx, 'a>,
        parameters: &Vec<(String, TypeKind)>,
    ) -> Allocator {
        let mut allocated = Vec::new();
        match &self {
            mir::FunctionDefinitionKind::Local(block, _) => {
                for param in parameters {
                    let allocation = scope
                        .block
                        .build_named(
                            param.0.clone(),
                            reid_lib::Instr::Alloca(param.1.get_type(scope.type_values)),
                        )
                        .unwrap();
                    allocated.push(Allocation(param.0.clone(), param.1.clone(), allocation));
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
                    named_variable_ref.1.clone(),
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
            mir::ExprKind::Accessed(expression, _, _) => {
                allocated.extend(expression.allocate(scope));
            }
            mir::ExprKind::Array(expressions) => {
                for expression in expressions {
                    allocated.extend(expression.allocate(scope));
                }
            }
            mir::ExprKind::Struct(_, items) => {
                for (_, expression) in items {
                    allocated.extend(expression.allocate(scope));
                }
            }
            mir::ExprKind::Literal(_) => {}
            mir::ExprKind::BinOp(_, lhs, rhs, _) => {
                allocated.extend(lhs.allocate(scope));
                allocated.extend(rhs.allocate(scope));
            }
            mir::ExprKind::FunctionCall(FunctionCall { parameters, .. }) => {
                for param in parameters {
                    allocated.extend(param.allocate(scope));
                }
            }
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
            mir::ExprKind::AssociatedFunctionCall(_, FunctionCall { parameters, .. }) => {
                for param in parameters {
                    allocated.extend(param.allocate(scope));
                }
            }
        }

        allocated
    }
}
