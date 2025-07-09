/// This module contains relevant code for [`Pass`] and shared code between
/// passes. Passes can be performed on Reid MIR to e.g. typecheck the code.
use std::collections::HashMap;
use std::error::Error as STDError;

use super::*;

#[derive(thiserror::Error, Debug, Clone)]
pub enum SimplePassError {
    #[error("Function not defined: {0}")]
    FunctionAlreadyDefined(String),
    #[error("Variable not defined: {0}")]
    VariableAlreadyDefined(String),
}

#[derive(Debug, Clone)]
pub struct Error<TErr: STDError> {
    metadata: Metadata,
    kind: TErr,
}

impl<TErr: STDError> std::fmt::Display for Error<TErr> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error at {}: {}", self.metadata, self.kind)
    }
}

impl<TErr: STDError> STDError for Error<TErr> {
    fn source(&self) -> Option<&(dyn STDError + 'static)> {
        self.kind.source()
    }
}

#[derive(Clone, Debug)]
pub struct Storage<T: std::fmt::Debug>(HashMap<String, T>);

#[derive(Debug)]
pub struct State<TErr: STDError> {
    pub errors: Vec<Error<TErr>>,
}

impl<TErr: STDError> State<TErr> {
    fn new() -> State<TErr> {
        State {
            errors: Default::default(),
        }
    }

    fn or_else<U, T: Into<Metadata> + Clone + Copy>(
        &mut self,
        result: Result<U, TErr>,
        default: U,
        meta: T,
    ) -> U {
        match result {
            Ok(t) => t,
            Err(e) => {
                self.errors.push(Error {
                    metadata: meta.into(),
                    kind: e,
                });
                default
            }
        }
    }

    fn ok<T: Into<Metadata> + Clone + Copy, U>(
        &mut self,
        result: Result<U, TErr>,
        meta: T,
    ) -> Option<U> {
        match result {
            Ok(v) => Some(v),
            Err(e) => {
                self.errors.push(Error {
                    metadata: meta.into(),
                    kind: e,
                });
                None
            }
        }
    }
}

impl<T: std::fmt::Debug> Default for Storage<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: Clone + std::fmt::Debug> Storage<T> {
    pub fn set(&mut self, key: String, value: T) -> Result<T, ()> {
        if let Some(_) = self.0.get(&key) {
            Err(())
        } else {
            self.0.insert(key, value.clone());
            Ok(value)
        }
    }

    pub fn get(&self, key: &String) -> Option<&T> {
        self.0.get(key)
    }
}

#[derive(Clone, Default, Debug)]
pub struct Scope {
    pub function_returns: Storage<ScopeFunction>,
    pub variables: Storage<TypeKind>,
    /// Hard Return type of this scope, if inside a function
    pub return_type_hint: Option<TypeKind>,
}

#[derive(Clone, Debug)]
pub struct ScopeFunction {
    pub ret: TypeKind,
    pub params: Vec<TypeKind>,
}

impl Scope {
    pub fn inner(&self) -> Scope {
        Scope {
            function_returns: self.function_returns.clone(),
            variables: self.variables.clone(),
            return_type_hint: self.return_type_hint,
        }
    }
}

pub struct PassState<'st, 'sc, TError: STDError + Clone> {
    state: &'st mut State<TError>,
    pub scope: &'sc mut Scope,
    inner: Vec<Scope>,
}

impl<'st, 'sc, TError: STDError + Clone> PassState<'st, 'sc, TError> {
    fn from(state: &'st mut State<TError>, scope: &'sc mut Scope) -> Self {
        PassState {
            state,
            scope,
            inner: Vec::new(),
        }
    }

    pub fn or_else<U, TMeta: Into<Metadata> + Clone + Copy>(
        &mut self,
        result: Result<U, TError>,
        default: U,
        meta: TMeta,
    ) -> U {
        self.state.or_else(result, default, meta)
    }

    pub fn ok<TMeta: Into<Metadata> + Clone + Copy, U>(
        &mut self,
        result: Result<U, TError>,
        meta: TMeta,
    ) -> Option<U> {
        self.state.ok(result, meta)
    }

    pub fn inner(&mut self) -> PassState<TError> {
        self.inner.push(self.scope.inner());
        let scope = self.inner.last_mut().unwrap();
        PassState {
            state: self.state,
            scope,
            inner: Vec::new(),
        }
    }
}

pub trait Pass {
    type TError: STDError + Clone;

    fn module(&mut self, _module: &mut Module, mut _state: PassState<Self::TError>) {}
    fn function(
        &mut self,
        _function: &mut FunctionDefinition,
        mut _state: PassState<Self::TError>,
    ) {
    }
    fn block(&mut self, _block: &mut Block, mut _state: PassState<Self::TError>) {}
    fn stmt(&mut self, _stmt: &mut Statement, mut _state: PassState<Self::TError>) {}
    fn expr(&mut self, _expr: &mut Expression, mut _state: PassState<Self::TError>) {}
}

impl Context {
    pub fn pass<T: Pass>(&mut self, pass: &mut T) -> State<T::TError> {
        let mut state = State::new();
        let mut scope = Scope::default();
        for module in &mut self.modules {
            module.pass(pass, &mut state, &mut scope);
        }
        state
    }
}

impl Module {
    fn pass<T: Pass>(&mut self, pass: &mut T, state: &mut State<T::TError>, scope: &mut Scope) {
        for function in &self.functions {
            scope
                .function_returns
                .set(
                    function.name.clone(),
                    ScopeFunction {
                        ret: function.return_type,
                        params: function.parameters.iter().map(|v| v.1).collect(),
                    },
                )
                .ok();
        }

        pass.module(self, PassState::from(state, scope));

        for function in &mut self.functions {
            function.pass(pass, state, scope);
        }
    }
}

impl FunctionDefinition {
    fn pass<T: Pass>(&mut self, pass: &mut T, state: &mut State<T::TError>, scope: &mut Scope) {
        for param in &self.parameters {
            scope.variables.set(param.0.clone(), param.1).ok();
        }

        pass.function(self, PassState::from(state, scope));

        match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                scope.return_type_hint = Some(self.return_type);
                block.pass(pass, state, scope);
            }
            FunctionDefinitionKind::Extern => {}
        };
    }
}

impl Block {
    fn pass<T: Pass>(&mut self, pass: &mut T, state: &mut State<T::TError>, scope: &mut Scope) {
        let mut scope = scope.inner();

        for statement in &mut self.statements {
            statement.pass(pass, state, &mut scope);
        }

        pass.block(self, PassState::from(state, &mut scope));
    }
}

impl Statement {
    fn pass<T: Pass>(&mut self, pass: &mut T, state: &mut State<T::TError>, scope: &mut Scope) {
        match &mut self.0 {
            StmtKind::Let(_, expression) => {
                expression.pass(pass, state, scope);
            }
            StmtKind::Import(_) => todo!(),
            StmtKind::Expression(expression) => {
                expression.pass(pass, state, scope);
            }
        }

        pass.stmt(self, PassState::from(state, scope));

        match &mut self.0 {
            StmtKind::Let(variable_reference, _) => scope
                .variables
                .set(variable_reference.1.clone(), variable_reference.0)
                .ok(),
            StmtKind::Import(_) => todo!(),
            StmtKind::Expression(_) => None,
        };
    }
}

impl Expression {
    fn pass<T: Pass>(&mut self, pass: &mut T, state: &mut State<T::TError>, scope: &mut Scope) {
        pass.expr(self, PassState::from(state, scope));
    }
}
