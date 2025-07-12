use std::{cell::RefCell, collections::HashMap};

use super::{
    typecheck::{Collapsable, ErrorKind},
    TypeKind,
};

#[derive(Debug, Clone)]
pub struct ScopeHint<'scope>(usize, &'scope ScopeHints<'scope>);

impl<'scope> ScopeHint<'scope> {
    pub fn resolve(&self) -> TypeRef {
        let mut scope = self.1;
        while !scope.type_hints.borrow().contains_key(&self.0) {
            scope = scope.outer.as_ref().unwrap();
        }
        let ty = scope.type_hints.borrow().get(&self.0).unwrap().clone();
        match ty.known() {
            Ok(narrow) => TypeRef::Literal(narrow),
            Err(_) => TypeRef::Hint(self.clone()),
        }
    }

    pub fn narrow(&self, ty_ref: &TypeRef) -> Result<ScopeHint, ErrorKind> {
        match ty_ref {
            TypeRef::Hint(other) => self.1.combine_vars(self, other),
            TypeRef::Literal(ty) => self.1.narrow_to_type(self, ty),
        }
    }
}

#[derive(Debug, Default)]
pub struct ScopeHints<'outer> {
    outer: Option<&'outer ScopeHints<'outer>>,
    /// Mapping of what types variables point to
    variables: RefCell<HashMap<String, (bool, usize)>>,
    /// Simple list of types that variables can refrence
    type_hints: RefCell<HashMap<usize, TypeKind>>,
}

impl<'outer> ScopeHints<'outer> {
    fn get_idx(&self) -> usize {
        self.type_hints.borrow().len() + self.outer.as_ref().map(|o| o.get_idx()).unwrap_or(0)
    }

    pub fn new_var(
        &'outer self,
        name: String,
        mutable: bool,
        initial_ty: TypeKind,
    ) -> Result<ScopeHint<'outer>, ErrorKind> {
        if self.variables.borrow().contains_key(&name) {
            return Err(ErrorKind::VariableAlreadyDefined(name));
        }
        let idx = self.get_idx();
        self.variables.borrow_mut().insert(name, (mutable, idx));
        self.type_hints.borrow_mut().insert(idx, initial_ty);
        Ok(ScopeHint(idx, self))
    }

    fn narrow_to_type(
        &'outer self,
        hint: &ScopeHint,
        ty: &TypeKind,
    ) -> Result<ScopeHint<'outer>, ErrorKind> {
        let mut hints = self.type_hints.borrow_mut();
        let existing = hints.get_mut(&hint.0).unwrap();
        *existing = existing.collapse_into(&ty)?;
        Ok(ScopeHint(hint.0, self))
    }

    fn combine_vars(
        &'outer self,
        hint1: &ScopeHint,
        hint2: &ScopeHint,
    ) -> Result<ScopeHint<'outer>, ErrorKind> {
        let ty = self.type_hints.borrow().get(&hint2.0).unwrap().clone();
        self.narrow_to_type(&hint1, &ty)?;
        for (_, (_, idx)) in self.variables.borrow_mut().iter_mut() {
            if *idx == hint2.0 {
                *idx = hint1.0;
            }
        }
        Ok(ScopeHint(hint1.0, self))
    }

    pub fn inner(&'outer self) -> ScopeHints<'outer> {
        ScopeHints {
            outer: Some(self),
            variables: Default::default(),
            type_hints: Default::default(),
        }
    }

    pub fn find_hint(&'outer self, name: &String) -> Option<(bool, ScopeHint<'outer>)> {
        self.variables
            .borrow()
            .get(name)
            .map(|(mutable, idx)| (*mutable, ScopeHint(*idx, self)))
    }
}

pub enum TypeRef<'scope> {
    Hint(ScopeHint<'scope>),
    Literal(TypeKind),
}

impl<'scope> TypeRef<'scope> {
    pub fn narrow(
        &'scope self,
        other: &'scope TypeRef<'scope>,
    ) -> Result<TypeRef<'scope>, ErrorKind> {
        match (self, other) {
            (TypeRef::Hint(hint), unk) | (unk, TypeRef::Hint(hint)) => {
                Ok(TypeRef::Hint(hint.narrow(unk)?))
            }
            (TypeRef::Literal(lit1), TypeRef::Literal(lit2)) => {
                Ok(TypeRef::Literal(lit1.collapse_into(lit2)?))
            }
        }
    }
}
