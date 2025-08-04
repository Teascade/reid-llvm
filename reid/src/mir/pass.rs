//! This module contains relevant code for [`Pass`] and shared code between
//! passes. Passes can be performed on Reid MIR to e.g. typecheck the code.

use std::collections::HashMap;
use std::convert::Infallible;
use std::error::Error as STDError;

use crate::codegen::intrinsics::{form_intrinsic_binops, get_intrinsic_assoc_func};
use crate::error_raporting::ReidError;

use super::*;

#[derive(thiserror::Error, Debug, Clone)]
pub enum SimplePassError {
    #[error("Function not defined: {0}")]
    FunctionAlreadyDefined(String),
    #[error("Variable not defined: {0}")]
    VariableAlreadyDefined(String),
}

#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
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

impl<TErr: STDError + PartialEq> PartialEq for Error<TErr> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.metadata.complete_overlap(&other.metadata)
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

    fn or_else<U, T: Into<Metadata> + Clone + Copy>(&mut self, result: Result<U, TErr>, default: U, meta: T) -> U {
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

    fn ok<T: Into<Metadata> + Clone + Copy, U>(&mut self, result: Result<U, TErr>, meta: T) -> Option<U> {
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
pub struct Storage<Key: std::hash::Hash, T: std::fmt::Debug>(HashMap<Key, T>);

impl<Key: std::hash::Hash, T: std::fmt::Debug> Default for Storage<Key, T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: std::hash::Hash + Eq, T: Clone + std::fmt::Debug> Storage<Key, T> {
    pub fn set(&mut self, key: Key, value: T) -> Result<T, ()> {
        if let Some(_) = self.0.get(&key) {
            Err(())
        } else {
            self.0.insert(key, value.clone());
            Ok(value)
        }
    }

    pub fn get(&self, key: &Key) -> Option<&T> {
        self.0.get(key)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Key, &T)> {
        self.0.iter()
    }

    pub fn find(&self, key: &Key) -> Option<(&Key, &T)> {
        self.0.iter().find(|(k, _)| *k == key)
    }

    pub fn filter(&self, key: &Key) -> Vec<(&Key, &T)> {
        self.0.iter().filter(|(k, _)| *k == key).collect()
    }
}

pub type BinopMap = Storage<BinopKey, ScopeBinopDef>;

#[derive(Clone, Default, Debug)]
pub struct Scope<Data: Clone + Default> {
    pub module_id: Option<SourceModuleId>,
    pub binops: BinopMap,
    pub associated_functions: Storage<AssociatedFunctionKey, ScopeFunction>,
    pub functions: Storage<String, ScopeFunction>,
    pub variables: Storage<String, ScopeVariable>,
    pub types: Storage<CustomTypeKey, TypeDefinition>,
    /// Hard Return type of this scope, if inside a function
    pub return_type_hint: Option<TypeKind>,
    pub data: Data,
}

impl<Data: Clone + Default> Scope<Data> {
    pub fn inner(&self) -> Scope<Data> {
        Scope {
            module_id: self.module_id,
            associated_functions: self.associated_functions.clone(),
            functions: self.functions.clone(),
            variables: self.variables.clone(),
            binops: self.binops.clone(),
            types: self.types.clone(),
            return_type_hint: self.return_type_hint.clone(),
            data: self.data.clone(),
        }
    }

    pub fn get_struct_type(&self, key: &CustomTypeKey) -> Option<&StructType> {
        let ty = self.types.get(&key)?;
        match &ty.kind {
            TypeDefinitionKind::Struct(struct_ty) => Some(struct_ty),
        }
    }

    pub fn find_type(&self, name: &String) -> Option<&CustomTypeKey> {
        self.types
            .0
            .iter()
            .find(|(CustomTypeKey(n, _), _)| n == name)
            .map(|(key, _)| key)
    }

    pub fn get_type(&self, typekey: &CustomTypeKey) -> Option<&TypeDefinition> {
        self.types.get(&typekey).or(self
            .types
            .0
            .iter()
            .find(|(key, def)| key.0 == typekey.0 && def.importer == Some(typekey.1))
            .map(|(_, v)| v))
    }

    pub fn get_associated_function(&mut self, key: &AssociatedFunctionKey) -> Option<ScopeFunction> {
        let ty = if let TypeKind::Borrow(inner, _) = &key.0 {
            *inner.clone()
        } else {
            key.0.clone()
        };
        let key = AssociatedFunctionKey(ty, key.1.clone());

        let func = self.associated_functions.get(&key);
        if let Some(func) = func {
            Some(func.clone())
        } else if let Some(func) = get_intrinsic_assoc_func(&key.0, &key.1) {
            self.associated_functions
                .set(
                    key.clone(),
                    ScopeFunction {
                        ret: func.return_type,
                        params: func.parameters.iter().map(|p| p.ty.clone()).collect(),
                    },
                )
                .unwrap();
            self.associated_functions.get(&key).cloned()
        } else {
            None
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AssociatedFunctionKey(pub TypeKind, pub String);

#[derive(Clone, Debug, Eq)]
pub struct BinopKey {
    pub params: (TypeKind, TypeKind),
    pub operator: BinaryOperator,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum CommutativeKind {
    True,
    False,
    Any,
}

impl PartialEq for BinopKey {
    fn eq(&self, other: &Self) -> bool {
        if self.operator != other.operator {
            return false;
        }
        if self.operator.is_commutative() != other.operator.is_commutative() {
            return false;
        }

        let operators_eq =
            self.params.0.narrow_into(&other.params.0).is_ok() && self.params.1.narrow_into(&other.params.1).is_ok();
        let swapped_ops_eq =
            self.params.0.narrow_into(&other.params.1).is_ok() && self.params.1.narrow_into(&other.params.0).is_ok();

        if self.operator.is_commutative() {
            operators_eq || swapped_ops_eq
        } else {
            operators_eq
        }
    }
}

impl std::hash::Hash for BinopKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.operator.is_commutative() {
            let mut sorted = vec![&self.params.0, &self.params.1];
            sorted.sort();
            sorted.hash(state);
            self.operator.hash(state);
        } else {
            self.params.hash(state);
        }
    }
}

#[derive(Clone, Debug)]
pub struct ScopeBinopDef {
    pub hands: (TypeKind, TypeKind),
    pub operator: BinaryOperator,
    pub return_ty: TypeKind,
}

impl ScopeBinopDef {
    pub fn narrow(&self, lhs: &TypeKind, rhs: &TypeKind) -> Option<(TypeKind, TypeKind, TypeKind)> {
        let lhs_ty = lhs.narrow_into(&self.hands.0);
        let rhs_ty = rhs.narrow_into(&self.hands.1);
        if let (Ok(lhs_ty), Ok(rhs_ty)) = (lhs_ty, rhs_ty) {
            Some((lhs_ty, rhs_ty, self.return_ty.clone()))
        } else {
            None
        }
    }
}

pub struct PassState<'st, 'sc, Data: Clone + Default, TError: STDError + Clone> {
    state: &'st mut State<TError>,
    pub scope: &'sc mut Scope<Data>,
    inner: Vec<Scope<Data>>,
    pub module_id: Option<SourceModuleId>,
}

impl<'st, 'sc, Data: Clone + Default, TError: STDError + Clone> PassState<'st, 'sc, Data, TError> {
    fn from(state: &'st mut State<TError>, scope: &'sc mut Scope<Data>, module_id: Option<SourceModuleId>) -> Self {
        PassState {
            state,
            scope,
            inner: Vec::new(),
            module_id: module_id,
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

    pub fn ok<TMeta: Into<Metadata> + Clone + Copy, U>(&mut self, result: Result<U, TError>, meta: TMeta) -> Option<U> {
        self.state.ok(result, meta)
    }

    pub fn note_errors<TMeta: Into<Metadata> + Clone>(&mut self, errors: &Vec<TError>, meta: TMeta) {
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
            module_id: self.module_id,
        }
    }
}

pub type PassResult = Result<(), ReidError>;

pub trait Pass {
    type Data: Clone + Default;
    type TError: STDError + Clone;

    fn context(&mut self, _context: &mut Context, mut _state: PassState<Self::Data, Self::TError>) -> PassResult {
        Ok(())
    }
    fn module(&mut self, _module: &mut Module, mut _state: PassState<Self::Data, Self::TError>) -> PassResult {
        Ok(())
    }
    fn function(
        &mut self,
        _function: &mut FunctionDefinition,
        mut _state: PassState<Self::Data, Self::TError>,
    ) -> PassResult {
        Ok(())
    }
    fn block(&mut self, _block: &mut Block, mut _state: PassState<Self::Data, Self::TError>) -> PassResult {
        Ok(())
    }
    fn stmt(&mut self, _stmt: &mut Statement, mut _state: PassState<Self::Data, Self::TError>) -> PassResult {
        Ok(())
    }
    fn expr(&mut self, _expr: &mut Expression, mut _state: PassState<Self::Data, Self::TError>) -> PassResult {
        Ok(())
    }
}

impl Context {
    pub fn pass<T: Pass>(&mut self, pass: &mut T) -> Result<State<T::TError>, ReidError> {
        let mut state = State::new();
        let mut scope = Scope::default();
        pass.context(self, PassState::from(&mut state, &mut scope, None))?;

        for intrinsic in form_intrinsic_binops() {
            scope
                .binops
                .set(
                    BinopKey {
                        params: (intrinsic.lhs.ty.clone(), intrinsic.rhs.ty.clone()),
                        operator: intrinsic.op,
                    },
                    ScopeBinopDef {
                        hands: (intrinsic.lhs.ty.clone(), intrinsic.rhs.ty.clone()),
                        operator: intrinsic.op,
                        return_ty: intrinsic.return_type.clone(),
                    },
                )
                .ok();
        }

        for (_, module) in &mut self.modules {
            module.pass(pass, &mut state, &mut scope.inner())?;
        }
        Ok(state)
    }
}

impl Module {
    fn pass<T: Pass>(&mut self, pass: &mut T, state: &mut State<T::TError>, scope: &mut Scope<T::Data>) -> PassResult {
        scope.module_id = Some(self.module_id);

        for typedef in &self.typedefs {
            scope
                .types
                .set(
                    CustomTypeKey(typedef.name.clone(), typedef.source_module),
                    typedef.clone(),
                )
                .ok();
        }

        for binop in &self.binop_defs {
            scope
                .binops
                .set(
                    BinopKey {
                        params: (binop.lhs.ty.clone(), binop.rhs.ty.clone()),
                        operator: binop.op,
                    },
                    ScopeBinopDef {
                        hands: (binop.lhs.ty.clone(), binop.rhs.ty.clone()),
                        operator: binop.op,
                        return_ty: binop.return_type.clone(),
                    },
                )
                .ok();
        }

        for function in &self.functions {
            scope
                .functions
                .set(
                    function.name.clone(),
                    ScopeFunction {
                        ret: function.return_type.clone(),
                        params: function.parameters.iter().cloned().map(|v| v.ty).collect(),
                    },
                )
                .ok();
        }

        for (ty, function) in &self.associated_functions {
            scope
                .associated_functions
                .set(
                    AssociatedFunctionKey(ty.clone(), function.name.clone()),
                    ScopeFunction {
                        ret: function.return_type.clone(),
                        params: function.parameters.iter().cloned().map(|v| v.ty).collect(),
                    },
                )
                .ok();
        }

        pass.module(self, PassState::from(state, scope, Some(self.module_id)))?;

        for function in &mut self.functions {
            function.pass(pass, state, &mut scope.inner(), self.module_id)?;
        }

        for (_, function) in &mut self.associated_functions {
            function.pass(pass, state, &mut scope.inner(), self.module_id)?;
        }
        Ok(())
    }
}

impl FunctionDefinition {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
        mod_id: SourceModuleId,
    ) -> PassResult {
        for param in &self.parameters {
            scope
                .variables
                .set(
                    param.name.clone(),
                    ScopeVariable {
                        ty: param.ty.clone(),
                        mutable: false,
                    },
                )
                .ok();
        }

        pass.function(self, PassState::from(state, scope, Some(mod_id)))?;

        match &mut self.kind {
            FunctionDefinitionKind::Local(block, _) => {
                scope.return_type_hint = Some(self.return_type.clone());
                block.pass(pass, state, scope, mod_id)?;
            }
            FunctionDefinitionKind::Extern(_) => {}
            FunctionDefinitionKind::Intrinsic(..) => {}
        };
        Ok(())
    }
}

impl Block {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
        mod_id: SourceModuleId,
    ) -> PassResult {
        let mut scope = scope.inner();

        for statement in &mut self.statements {
            statement.pass(pass, state, &mut scope, mod_id)?;
        }

        if let Some((_, Some(return_expr))) = &mut self.return_expression {
            return_expr.pass(pass, state, &mut scope, mod_id)?;
        }

        pass.block(self, PassState::from(state, &mut scope, Some(mod_id)))
    }
}

impl Statement {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
        mod_id: SourceModuleId,
    ) -> PassResult {
        match &mut self.0 {
            StmtKind::Let(_, _, expression) => {
                expression.pass(pass, state, scope, mod_id)?;
            }
            StmtKind::Set(set_expr, expression) => {
                set_expr.pass(pass, state, scope, mod_id)?;
                expression.pass(pass, state, scope, mod_id)?;
            }
            StmtKind::Import(_) => {}
            StmtKind::Expression(expression) => {
                expression.pass(pass, state, scope, mod_id)?;
            }
            StmtKind::While(while_statement) => {
                while_statement.condition.pass(pass, state, scope, mod_id)?;
                while_statement.block.pass(pass, state, scope, mod_id)?;
            }
        }

        pass.stmt(self, PassState::from(state, scope, Some(mod_id)))?;

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
            StmtKind::Import(_) => {}
            StmtKind::Expression(_) => {}
            StmtKind::While(_) => {}
        };
        Ok(())
    }
}

impl Expression {
    fn pass<T: Pass>(
        &mut self,
        pass: &mut T,
        state: &mut State<T::TError>,
        scope: &mut Scope<T::Data>,
        mod_id: SourceModuleId,
    ) -> PassResult {
        pass.expr(self, PassState::from(state, scope, Some(mod_id)))?;
        match &mut self.0 {
            ExprKind::Variable(_) => {}
            ExprKind::Indexed(value_expr, _, index_expr) => {
                value_expr.pass(pass, state, scope, mod_id)?;
                index_expr.pass(pass, state, scope, mod_id)?;
            }
            ExprKind::Accessed(value_expr, ..) => {
                value_expr.pass(pass, state, scope, mod_id)?;
            }
            ExprKind::Array(expressions) => {
                for expr in expressions {
                    expr.pass(pass, state, scope, mod_id)?;
                }
            }
            ExprKind::Struct(_, items) => {
                for (_, expr, _) in items {
                    expr.pass(pass, state, scope, mod_id)?;
                }
            }
            ExprKind::Literal(_) => {}
            ExprKind::BinOp(_, lhs, rhs, _) => {
                lhs.pass(pass, state, scope, mod_id)?;
                rhs.pass(pass, state, scope, mod_id)?;
            }
            ExprKind::FunctionCall(FunctionCall { parameters, .. }) => {
                for expr in parameters {
                    expr.pass(pass, state, scope, mod_id)?;
                }
            }
            ExprKind::AssociatedFunctionCall(_, FunctionCall { parameters, .. }) => {
                for expr in parameters {
                    expr.pass(pass, state, scope, mod_id)?;
                }
            }
            ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                cond.pass(pass, state, scope, mod_id)?;
                lhs.pass(pass, state, scope, mod_id)?;
                if let Some(rhs) = rhs.as_mut() {
                    rhs.pass(pass, state, scope, mod_id)?;
                }
            }
            ExprKind::Block(block) => block.pass(pass, state, scope, mod_id)?,
            ExprKind::Borrow(expression, _) => expression.pass(pass, state, scope, mod_id)?,
            ExprKind::Deref(expression) => expression.pass(pass, state, scope, mod_id)?,
            ExprKind::CastTo(expression, _) => expression.pass(pass, state, scope, mod_id)?,
            ExprKind::GlobalRef(..) => {}
        }
        Ok(())
    }
}
