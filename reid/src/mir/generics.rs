use std::{collections::HashMap, path::PathBuf};

use log::Metadata;

use crate::mir::{
    self, generics, CustomTypeKey, FunctionCall, FunctionDefinition, FunctionParam, GlobalKind, GlobalValue,
    IfExpression, Literal, Module, SourceModuleId, TypeKind, WhileStatement,
};

use super::pass::{Pass, PassResult, PassState};

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorKind {
    #[error("Should never be encountered!")]
    Null,
    #[error("Expected {0} type-arguments, got {1}!")]
    InvalidNumberTypeArguments(u32, u32),
}

type Calls = Vec<(Vec<TypeKind>, mir::Metadata)>;

pub struct GenericsPass {
    pub function_map: HashMap<SourceModuleId, Functions>,
}

#[derive(Debug)]
pub struct Functions {
    calls: HashMap<String, Calls>,
    assoc_calls: HashMap<(TypeKind, String), Calls>,
}

#[derive(Default, Clone)]
pub struct GenericsPassData {
    generic_types: HashMap<String, TypeKind>,
}

type GenericsPassState<'map, 'st, 'sc> = PassState<'st, 'sc, GenericsPassData, ErrorKind>;

impl Pass for GenericsPass {
    type Data = GenericsPassData;
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

        for module in &mut context.modules {
            let mut calls = HashMap::new();
            let mut assoc_calls = HashMap::new();
            for function in &mut module.1.associated_functions {
                match &mut function.1.kind {
                    mir::FunctionDefinitionKind::Local(block, _) => block.find_calls(&mut calls, &mut assoc_calls),
                    mir::FunctionDefinitionKind::Extern(_) => {}
                    mir::FunctionDefinitionKind::Intrinsic(_) => {}
                }
            }
            for function in &mut module.1.functions {
                match &mut function.kind {
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

        self.function_map = function_map;

        Ok(())
    }

    fn module(&mut self, module: &mut mir::Module, mut state: PassState<Self::Data, Self::TError>) -> PassResult {
        for function in module.functions.drain(..).collect::<Vec<_>>() {
            if let Some(source) = function.source {
                let functions = self.function_map.get(&source).unwrap();
                let calls = functions.calls.get(&function.name).unwrap();

                for call in calls {
                    if call.0.len() != function.generics.len() {
                        state.note_errors(
                            &vec![ErrorKind::InvalidNumberTypeArguments(
                                function.generics.len() as u32,
                                call.0.len() as u32,
                            )],
                            call.1,
                        );
                    }
                }

                if function.generics.len() > 0 {
                    for call in calls {
                        if let Some(clone) = function.try_clone() {
                            let generics = function
                                .generics
                                .iter()
                                .zip(call.0.clone())
                                .map(|((n, _), t)| (n.clone(), t.clone()))
                                .collect();
                            module.functions.push(FunctionDefinition {
                                name: name_fmt(function.name.clone(), call.0.clone()),
                                return_type: function.return_type.replace_generic(&generics),
                                parameters: function
                                    .parameters
                                    .iter()
                                    .map(|p| FunctionParam {
                                        ty: p.ty.replace_generic(&generics),
                                        ..p.clone()
                                    })
                                    .collect(),
                                generics,
                                ..clone
                            });
                        }
                    }
                } else {
                    module.functions.push(function);
                }
            } else {
                module.functions.push(function);
            }
        }
        Ok(())
    }

    fn function(
        &mut self,
        func: &mut FunctionDefinition,
        mut state: PassState<Self::Data, Self::TError>,
    ) -> PassResult {
        for (name, ty) in &func.generics {
            state.scope.data.generic_types.insert(name.clone(), ty.clone());
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &mut mir::Statement, mut state: PassState<Self::Data, Self::TError>) -> PassResult {
        match &mut stmt.0 {
            mir::StmtKind::Let(var_ref, _, _) => match var_ref.0.clone() {
                TypeKind::CustomType(custom_type_key) => {
                    if let Some(ty) = state.scope.data.generic_types.get(&custom_type_key.0) {
                        var_ref.0 = ty.clone();
                    }
                }
                _ => {}
            },
            _ => {}
        }
        Ok(())
    }
}

impl mir::Block {
    fn find_calls(&mut self, calls: &mut HashMap<String, Calls>, assoc_calls: &mut HashMap<(TypeKind, String), Calls>) {
        for statement in &mut self.statements {
            statement.find_calls(calls, assoc_calls);
        }
        if let Some((_, Some(e))) = &mut self.return_expression {
            e.find_calls(calls, assoc_calls);
        }
    }
}

impl mir::Statement {
    fn find_calls(&mut self, calls: &mut HashMap<String, Calls>, assoc_calls: &mut HashMap<(TypeKind, String), Calls>) {
        match &mut self.0 {
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
    fn find_calls(&mut self, calls: &mut HashMap<String, Calls>, assoc_calls: &mut HashMap<(TypeKind, String), Calls>) {
        match &mut self.0 {
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
            mir::ExprKind::Literal(_) => {}
            mir::ExprKind::BinOp(_, lhs, rhs, _) => {
                lhs.find_calls(calls, assoc_calls);
                rhs.find_calls(calls, assoc_calls);
            }
            mir::ExprKind::FunctionCall(function_call) => {
                if let Some(calls) = calls.get_mut(&function_call.name) {
                    calls.push((function_call.generics.clone(), self.1));
                } else {
                    calls.insert(
                        function_call.name.clone(),
                        vec![(function_call.generics.clone(), self.1)],
                    );
                }
                if function_call.generics.len() > 0 {
                    function_call.name = name_fmt(function_call.name.clone(), function_call.generics.clone())
                }
            }
            mir::ExprKind::AssociatedFunctionCall(ty, function_call) => {
                if let Some(calls) = assoc_calls.get_mut(&(ty.clone(), function_call.name.clone())) {
                    calls.push((function_call.generics.clone(), self.1));
                } else {
                    assoc_calls.insert(
                        (ty.clone(), function_call.name.clone()),
                        vec![(function_call.generics.clone(), self.1)],
                    );
                }
                if function_call.generics.len() > 0 {
                    function_call.name = name_fmt(function_call.name.clone(), function_call.generics.clone())
                }
            }
            mir::ExprKind::If(IfExpression(cond, then_e, else_e)) => {
                cond.find_calls(calls, assoc_calls);
                then_e.find_calls(calls, assoc_calls);
                if let Some(else_e) = else_e.as_mut() {
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

fn name_fmt(name: String, generics: Vec<TypeKind>) -> String {
    format!(
        "{}.{}",
        name,
        generics.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(".")
    )
}

impl TypeKind {
    fn replace_generic(&self, generics: &Vec<(String, TypeKind)>) -> TypeKind {
        match self {
            TypeKind::CustomType(CustomTypeKey(name, _)) => {
                if let Some((_, inner)) = generics.iter().find(|(n, _)| n == name) {
                    inner.clone()
                } else {
                    self.clone()
                }
            }
            _ => self.clone(),
        }
    }
}
