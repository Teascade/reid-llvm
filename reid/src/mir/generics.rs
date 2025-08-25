use std::{collections::HashMap, path::PathBuf};

use crate::mir::{
    self, FunctionCall, GlobalKind, GlobalValue, IfExpression, Literal, Module, SourceModuleId, TypeKind,
    WhileStatement,
};

use super::pass::{Pass, PassResult, PassState};

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorKind {
    #[error("Should never be encountered!")]
    Null,
}

type Calls = Vec<Vec<TypeKind>>;

pub struct GenericsPass {
    pub function_map: HashMap<SourceModuleId, Functions>,
}

#[derive(Debug)]
pub struct Functions {
    calls: HashMap<String, Calls>,
    assoc_calls: HashMap<(TypeKind, String), Calls>,
}

type GenericsPassState<'map, 'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

impl Pass for GenericsPass {
    type Data = ();
    type TError = ErrorKind;

    fn context(&mut self, context: &mut mir::Context, mut _state: PassState<Self::Data, Self::TError>) -> PassResult {
        let mut function_map = HashMap::new();
        for module in &context.modules {
            function_map.insert(
                module.0.clone(),
                Functions {
                    calls: HashMap::new(),
                    assoc_calls: HashMap::new(),
                },
            );
        }

        for module in &context.modules {
            let mut calls = HashMap::new();
            let mut assoc_calls = HashMap::new();
            for function in &module.1.associated_functions {
                match &function.1.kind {
                    mir::FunctionDefinitionKind::Local(block, _) => block.find_calls(&mut calls, &mut assoc_calls),
                    mir::FunctionDefinitionKind::Extern(_) => {}
                    mir::FunctionDefinitionKind::Intrinsic(_) => {}
                }
            }
            for function in &module.1.functions {
                match &function.kind {
                    mir::FunctionDefinitionKind::Local(block, _) => block.find_calls(&mut calls, &mut assoc_calls),
                    mir::FunctionDefinitionKind::Extern(_) => {}
                    mir::FunctionDefinitionKind::Intrinsic(_) => {}
                }
            }

            for function in &module.1.associated_functions {
                if let Some(source) = function.1.source {
                    let key = (function.0.clone(), function.1.name.clone());
                    function_map
                        .get_mut(&source)
                        .unwrap()
                        .assoc_calls
                        .insert(key.clone(), assoc_calls.get(&key).cloned().unwrap_or_default());
                }
            }
            for function in &module.1.functions {
                if let Some(source) = function.source {
                    function_map.get_mut(&source).unwrap().calls.insert(
                        function.name.clone(),
                        calls.get(&function.name).cloned().unwrap_or_default(),
                    );
                }
            }
        }
        dbg!(&function_map);

        self.function_map = function_map;

        Ok(())
    }

    fn module(&mut self, module: &mut mir::Module, mut state: PassState<Self::Data, Self::TError>) -> PassResult {
        Ok(())
    }
}

impl mir::Block {
    fn find_calls(&self, calls: &mut HashMap<String, Calls>, assoc_calls: &mut HashMap<(TypeKind, String), Calls>) {
        for statement in &self.statements {
            statement.find_calls(calls, assoc_calls);
        }
        if let Some((_, Some(e))) = &self.return_expression {
            e.find_calls(calls, assoc_calls);
        }
    }
}

impl mir::Statement {
    fn find_calls(&self, calls: &mut HashMap<String, Calls>, assoc_calls: &mut HashMap<(TypeKind, String), Calls>) {
        match &self.0 {
            mir::StmtKind::Let(_, _, expression) => expression.find_calls(calls, assoc_calls),
            mir::StmtKind::Set(expression, expression1) => {
                expression.find_calls(calls, assoc_calls);
                expression1.find_calls(calls, assoc_calls);
            }
            mir::StmtKind::Import(_) => {}
            mir::StmtKind::Expression(expression) => expression.find_calls(calls, assoc_calls),
            mir::StmtKind::While(WhileStatement { condition, block, .. }) => {
                condition.find_calls(calls, assoc_calls);
                block.find_calls(calls, assoc_calls);
            }
        }
    }
}

impl mir::Expression {
    fn find_calls(&self, calls: &mut HashMap<String, Calls>, assoc_calls: &mut HashMap<(TypeKind, String), Calls>) {
        match &self.0 {
            mir::ExprKind::Variable(_) => {}
            mir::ExprKind::Indexed(expression, _, expression1) => {
                expression.find_calls(calls, assoc_calls);
                expression1.find_calls(calls, assoc_calls);
            }
            mir::ExprKind::Accessed(expression, _, _, _) => {
                expression.find_calls(calls, assoc_calls);
            }
            mir::ExprKind::Array(expressions) => {
                for expression in expressions {
                    expression.find_calls(calls, assoc_calls);
                }
            }
            mir::ExprKind::Struct(_, items) => {
                for item in items {
                    item.1.find_calls(calls, assoc_calls);
                }
            }
            mir::ExprKind::Literal(_) => todo!(),
            mir::ExprKind::BinOp(_, lhs, rhs, _) => {
                lhs.find_calls(calls, assoc_calls);
                rhs.find_calls(calls, assoc_calls);
            }
            mir::ExprKind::FunctionCall(function_call) => {
                if let Some(calls) = calls.get_mut(&function_call.name) {
                    calls.push(function_call.generics.clone());
                } else {
                    calls.insert(function_call.name.clone(), vec![function_call.generics.clone()]);
                }
            }
            mir::ExprKind::AssociatedFunctionCall(ty, function_call) => {
                if let Some(calls) = assoc_calls.get_mut(&(ty.clone(), function_call.name.clone())) {
                    calls.push(function_call.generics.clone());
                } else {
                    assoc_calls.insert(
                        (ty.clone(), function_call.name.clone()),
                        vec![function_call.generics.clone()],
                    );
                }
            }
            mir::ExprKind::If(IfExpression(cond, then_e, else_e)) => {
                cond.find_calls(calls, assoc_calls);
                then_e.find_calls(calls, assoc_calls);
                if let Some(else_e) = else_e.as_ref() {
                    else_e.find_calls(calls, assoc_calls);
                }
            }
            mir::ExprKind::Block(block) => block.find_calls(calls, assoc_calls),
            mir::ExprKind::Borrow(expression, _) => expression.find_calls(calls, assoc_calls),
            mir::ExprKind::Deref(expression) => expression.find_calls(calls, assoc_calls),
            mir::ExprKind::CastTo(expression, _) => expression.find_calls(calls, assoc_calls),
            mir::ExprKind::GlobalRef(_, _) => {}
        }
    }
}
