//! This module contains relevant code for [`Pass`] and shared code between
//! passes. Passes can be performed on Reid MIR to e.g. typecheck the code.

use std::collections::HashMap;
use std::convert::Infallible;
use std::error::Error as STDError;

use super::*;

#[derive(thiserror::Error, Debug, Clone)]
pub enum SimplePassError {
    #[error("Function not defined: {0}")]
    FunctionAlreadyDefined(String),
    #[error("Variable not defined: {0}")]
    VariableAlreadyDefined(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Error<TErr: STDError> {
    pub metadata: Metadata,
    pub kind: TErr,
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

#[derive(Clone, Debug)]
pub struct Storage<T: std::fmt::Debug>(HashMap<String, T>);

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
pub struct Scope<Data: Clone + Default> {
    pub function_returns: Storage<ScopeFunction>,
    pub variables: Storage<ScopeVariable>,
    pub types: Storage<TypeDefinitionKind>,
    /// Hard Return type of this scope, if inside a function
    pub return_type_hint: Option<TypeKind>,
    pub data: Data,
}

impl<Data: Clone + Default> Scope<Data> {
    pub fn inner(&self) -> Scope<Data> {
        Scope {
            function_returns: self.function_returns.clone(),
            variables: self.variables.clone(),
            types: self.types.clone(),
            return_type_hint: self.return_type_hint.clone(),
            data: self.data.clone(),
        }
    }

    pub fn get_struct_type(&self, name: &String) -> Option<&StructType> {
        let ty = self.types.get(&name)?;
        match ty {
            TypeDefinitionKind::Struct(struct_ty) => Some(struct_ty),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ScopeFunction {
    pub ret: TypeKind,
    pub params: Vec<TypeKind>,
}

#[derive(Clone, Debug)]
pub struct ScopeVariable {
    pub ty: TypeKind,
    pub mutable: bool,
}

pub struct PassState<'st, 'sc, Data: Clone + Default, TError: STDError + Clone> {
    state: &'st mut State<TError>,
    pub scope: &'sc mut Scope<Data>,
    inner: Vec<Scope<Data>>,
}

impl<'st, 'sc, Data: Clone + Default, TError: STDError + Clone> PassState<'st, 'sc, Data, TError> {
    fn from(state: &'st mut State<TError>, scope: &'sc mut Scope<Data>) -> Self {
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

    pub fn note_errors<TMeta: Into<Metadata> + Clone>(
        &mut self,
        errors: &Vec<TError>,
        meta: TMeta,
    ) {
        for error in errors {
            self.ok::<_, Infallible>(Err(error.clone()), meta.clone().into());
        }
    }

    pub fn inner(&mut self) -> PassState<Data, TError> {
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
    type Data: Clone + Default;
    type TError: STDError + Clone;

    fn context(&mut self, _context: &mut Context, mut _state: PassState<Self::Data, Self::TError>) {
    }
    fn module(&mut self, _module: &mut Module, mut _state: PassState<Self::Data, Self::TError>) {}
    fn function(
        &mut self,
        _function: &mut FunctionDefinition,
        mut _state: PassState<Self::Data, Self::TError>,
    ) {
    }
    fn block(&mut self, _block: &mut Block, mut _state: PassState<Self::Data, Self::TError>) {}
    fn stmt(&mut self, _stmt: &mut Statement, mut _state: PassState<Self::Data, Self::TError>) {}
    fn expr(&mut self, _expr: &mut Expression, mut _state: PassState<Self::Data, Self::TError>) {}
}

impl Context {
    pub fn pass<T: Pass>(&mut self, pass: &mut T) -> State<T::TError> {
        let mut state = State::new();
        let mut scope = Scope::default();
        pass.context(self, PassState::from(&mut state, &mut scope));
        for module in &mut self.modules {
            module.pass(pass, &mut state, &mut scope.inner());
        }
        state
    }
}

impl Module {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
    ) {
        for typedef in &self.typedefs {
            let kind = match &typedef.kind {
                TypeDefinitionKind::Struct(fields) => TypeDefinitionKind::Struct(fields.clone()),
            };
            scope.types.set(typedef.name.clone(), kind).ok();
        }

        for function in &self.functions {
            scope
                .function_returns
                .set(
                    function.name.clone(),
                    ScopeFunction {
                        ret: function.return_type.clone(),
                        params: function.parameters.iter().cloned().map(|v| v.1).collect(),
                    },
                )
                .ok();
        }

        pass.module(self, PassState::from(state, scope));

        for function in &mut self.functions {
            function.pass(pass, state, &mut scope.inner());
        }
    }
}

impl FunctionDefinition {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
    ) {
        for param in &self.parameters {
            scope
                .variables
                .set(
                    param.0.clone(),
                    ScopeVariable {
                        ty: param.1.clone(),
                        mutable: false,
                    },
                )
                .ok();
        }

        pass.function(self, PassState::from(state, scope));

        match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                scope.return_type_hint = Some(self.return_type.clone());
                block.pass(pass, state, scope);
            }
            FunctionDefinitionKind::Extern(_) => {}
        };
    }
}

impl Block {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
    ) {
        let mut scope = scope.inner();

        for statement in &mut self.statements {
            statement.pass(pass, state, &mut scope);
        }

        pass.block(self, PassState::from(state, &mut scope));
    }
}

impl Statement {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
    ) {
        match &mut self.0 {
            StmtKind::Let(_, _, expression) => {
                expression.pass(pass, state, scope);
            }
            StmtKind::Set(_, expression) => {
                expression.pass(pass, state, scope);
            }
            StmtKind::Import(_) => {} // Never exists at this stage
            StmtKind::Expression(expression) => {
                expression.pass(pass, state, scope);
            }
        }

        pass.stmt(self, PassState::from(state, scope));

        match &mut self.0 {
            StmtKind::Let(variable_reference, mutable, _) => {
                scope
                    .variables
                    .set(
                        variable_reference.1.clone(),
                        ScopeVariable {
                            ty: variable_reference.0.clone(),
                            mutable: *mutable,
                        },
                    )
                    .ok();
            }
            StmtKind::Set(_, _) => {}
            StmtKind::Import(_) => {} // Never exists at this stage
            StmtKind::Expression(_) => {}
        };
    }
}

impl Expression {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
    ) {
        pass.expr(self, PassState::from(state, scope));
    }
}
