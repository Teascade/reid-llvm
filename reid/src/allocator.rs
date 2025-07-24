use std::collections::HashMap;

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    Block,
};

use crate::mir::{
    self, CustomTypeKey, FunctionCall, FunctionDefinition, IfExpression, SourceModuleId,
    TypeDefinition, TypeKind, WhileStatement,
};

#[derive(Debug)]
pub struct Allocator {
    allocations: Vec<Allocation>,
}

pub struct AllocatorScope<'ctx, 'a> {
    pub(super) block: &'a mut Block<'ctx>,
    pub(super) module_id: SourceModuleId,
    pub(super) type_values: &'a HashMap<CustomTypeKey, TypeValue>,
}

impl Allocator {
    pub fn empty() -> Allocator {
        Allocator {
            allocations: Vec::new(),
        }
    }

    pub fn from(func: &FunctionDefinition, scope: &mut AllocatorScope) -> Allocator {
        func.allocate(scope)
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

impl mir::FunctionDefinition {
    fn allocate<'ctx, 'a>(&self, scope: &mut AllocatorScope<'ctx, 'a>) -> Allocator {
        let mut allocated = Vec::new();
        match &self.kind {
            mir::FunctionDefinitionKind::Local(block, _) => {
                for param in &self.parameters {
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

        Allocator {
            allocations: allocated,
        }
    }
}

impl mir::Block {
    fn allocate<'ctx, 'a>(&self, scope: &mut AllocatorScope<'ctx, 'a>) -> Vec<Allocation> {
        let mut allocated = Vec::new();

        for statement in &self.statements {
            allocated.extend(statement.allocate(scope));
        }

        if let Some((_, ret_expr)) = &self.return_expression {
            allocated.extend(ret_expr.allocate(scope));
        }

        allocated
    }
}

impl mir::Statement {
    fn allocate<'ctx, 'a>(&self, scope: &mut AllocatorScope<'ctx, 'a>) -> Vec<Allocation> {
        let mut allocated = Vec::new();

        match &self.0 {
            crate::mir::StmtKind::Let(named_variable_ref, _, expression) => {
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
            crate::mir::StmtKind::Set(lhs, rhs) => {
                allocated.extend(lhs.allocate(scope));
                allocated.extend(rhs.allocate(scope));
            }
            crate::mir::StmtKind::Import(_) => {}
            crate::mir::StmtKind::Expression(expression) => {
                allocated.extend(expression.allocate(scope));
            }
            crate::mir::StmtKind::While(WhileStatement {
                condition, block, ..
            }) => {
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
            crate::mir::ExprKind::Variable(_) => {}
            crate::mir::ExprKind::Indexed(expr, _, idx) => {
                allocated.extend(expr.allocate(scope));
                allocated.extend(idx.allocate(scope));
            }
            crate::mir::ExprKind::Accessed(expression, _, _) => {
                allocated.extend(expression.allocate(scope));
            }
            crate::mir::ExprKind::Array(expressions) => {
                for expression in expressions {
                    allocated.extend(expression.allocate(scope));
                }
            }
            crate::mir::ExprKind::Struct(_, items) => {
                for (_, expression) in items {
                    allocated.extend(expression.allocate(scope));
                }
            }
            crate::mir::ExprKind::Literal(_) => {}
            crate::mir::ExprKind::BinOp(_, lhs, rhs) => {
                allocated.extend(lhs.allocate(scope));
                allocated.extend(rhs.allocate(scope));
            }
            crate::mir::ExprKind::FunctionCall(FunctionCall { parameters, .. }) => {
                for param in parameters {
                    allocated.extend(param.allocate(scope));
                }
            }
            crate::mir::ExprKind::If(IfExpression(cond, then_ex, else_ex)) => {
                allocated.extend(cond.allocate(scope));
                allocated.extend(then_ex.allocate(scope));
                if let Some(else_ex) = else_ex.as_ref() {
                    allocated.extend(else_ex.allocate(scope));
                }
            }
            crate::mir::ExprKind::Block(block) => {
                allocated.extend(block.allocate(scope));
            }
            crate::mir::ExprKind::Borrow(_, _) => {}
            crate::mir::ExprKind::Deref(_) => {}
            crate::mir::ExprKind::CastTo(expression, _) => {
                allocated.extend(expression.allocate(scope));
            }
        }

        allocated
    }
}
