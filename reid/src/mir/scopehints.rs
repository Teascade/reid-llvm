use std::{
    any::TypeId,
    cell::RefCell,
    collections::{HashMap, HashSet},
    hint::black_box,
    rc::Rc,
};

use super::{
    typecheck::{Collapsable, ErrorKind},
    BinaryOperator, TypeKind,
};

#[derive(Clone)]
pub struct ScopeHint<'scope>(TypeIdRef, &'scope ScopeHints<'scope>);

impl<'scope> ScopeHint<'scope> {
    pub unsafe fn resolve_type(&self) -> TypeKind {
        unsafe { *self.1.types.hints.borrow().get_unchecked(*self.0.borrow()) }
    }

    pub fn narrow(&mut self, other: &ScopeHint) -> Result<ScopeHint<'scope>, ErrorKind> {
        self.1.combine_vars(self, other)
    }

    pub fn as_type(&self) -> TypeKind {
        TypeKind::Vague(super::VagueType::Hinted(*self.0.borrow()))
    }
}

impl<'scope> std::fmt::Debug for ScopeHint<'scope> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Hint")
            .field(&self.0)
            .field(unsafe { &self.resolve_type() })
            .finish()
    }
}

type TypeIdRef = Rc<RefCell<usize>>;

#[derive(Debug, Default)]
pub struct TypeHints {
    /// Simple list of types that variables can refrence
    hints: RefCell<Vec<TypeKind>>,
    /// Indirect ID-references, referring to hints-vec
    type_refs: RefCell<Vec<TypeIdRef>>,
}

impl TypeHints {
    pub fn new(&self, ty: TypeKind) -> TypeIdRef {
        let idx = self.hints.borrow().len();
        let typecell = Rc::new(RefCell::new(idx));
        self.type_refs.borrow_mut().push(typecell.clone());
        self.hints.borrow_mut().push(ty);
        typecell
    }

    pub fn find(&self, ty: TypeKind) -> Option<TypeIdRef> {
        if ty.known().is_err() {
            // Only do this for non-vague types that can not be further narrowed
            // down.
            return None;
        }

        if let Some(idx) = self
            .hints
            .borrow_mut()
            .iter()
            .enumerate()
            .find(|(_, t)| **t == ty)
            .map(|(i, _)| i)
        {
            Some(Rc::new(RefCell::new(idx)))
        } else {
            None
        }
    }

    unsafe fn recurse_type_ref(&self, mut idx: usize) -> TypeIdRef {
        let refs = self.type_refs.borrow();
        let mut inner_idx = refs.get_unchecked(idx);
        let mut seen = HashSet::new();
        while (*inner_idx.borrow()) != idx && !seen.contains(&idx) {
            seen.insert(idx);
            idx = *inner_idx.borrow();
            inner_idx = refs.get_unchecked(idx);
        }
        return refs.get_unchecked(idx).clone();
    }
}

#[derive(Debug)]
pub struct ScopeHints<'outer> {
    types: &'outer TypeHints,
    outer: Option<&'outer ScopeHints<'outer>>,
    /// Mapping of what types variables point to
    variables: RefCell<HashMap<String, (bool, TypeIdRef)>>,
}

impl<'outer> ScopeHints<'outer> {
    pub fn from(types: &'outer TypeHints) -> ScopeHints<'outer> {
        ScopeHints {
            types,
            outer: Default::default(),
            variables: Default::default(),
        }
    }

    pub fn retrieve_type(&self, idx: usize) -> Option<TypeKind> {
        let inner_idx = unsafe { *self.types.recurse_type_ref(idx).borrow() };
        self.types.hints.borrow().get(inner_idx).copied()
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
        let idx = self.types.new(initial_ty);
        self.variables
            .borrow_mut()
            .insert(name, (mutable, idx.clone()));
        Ok(ScopeHint(idx, self))
    }

    pub fn from_type(&'outer self, ty: &TypeKind) -> Option<ScopeHint<'outer>> {
        let idx = match ty {
            TypeKind::Vague(super::VagueType::Hinted(idx)) => {
                let inner_idx = unsafe { *self.types.recurse_type_ref(*idx).borrow() };
                self.types.type_refs.borrow().get(inner_idx).cloned()?
            }
            TypeKind::Vague(_) => self.types.new(*ty),
            _ => {
                if let Some(ty_ref) = self.types.find(*ty) {
                    ty_ref
                } else {
                    self.types.new(*ty)
                }
            }
        };
        Some(ScopeHint(idx, self))
    }

    fn narrow_to_type(
        &'outer self,
        hint: &ScopeHint,
        ty: &TypeKind,
    ) -> Result<ScopeHint<'outer>, ErrorKind> {
        unsafe {
            let mut hints = self.types.hints.borrow_mut();
            let existing = hints.get_unchecked_mut(*hint.0.borrow());
            *existing = existing.collapse_into(&ty)?;
            Ok(ScopeHint(hint.0.clone(), self))
        }
    }

    fn combine_vars(
        &'outer self,
        hint1: &ScopeHint,
        hint2: &ScopeHint,
    ) -> Result<ScopeHint<'outer>, ErrorKind> {
        unsafe {
            let ty = self
                .types
                .hints
                .borrow()
                .get_unchecked(*hint2.0.borrow())
                .clone();
            self.narrow_to_type(&hint1, &ty)?;
            for idx in self.types.type_refs.borrow_mut().iter_mut() {
                if *idx == hint2.0 && idx != &hint1.0 {
                    *idx.borrow_mut() = *hint1.0.borrow();
                }
            }
            Ok(ScopeHint(hint1.0.clone(), self))
        }
    }

    pub fn inner(&'outer self) -> ScopeHints<'outer> {
        ScopeHints {
            types: self.types,
            outer: Some(self),
            variables: Default::default(),
        }
    }

    pub fn find_hint(&'outer self, name: &String) -> Option<(bool, ScopeHint<'outer>)> {
        self.variables
            .borrow()
            .get(name)
            .map(|(mutable, idx)| (*mutable, ScopeHint(idx.clone(), self)))
            .or(self.outer.map(|o| o.find_hint(name)).flatten())
    }

    pub fn binop(
        &'outer self,
        op: &BinaryOperator,
        lhs: &mut ScopeHint<'outer>,
        rhs: &mut ScopeHint<'outer>,
    ) -> Result<ScopeHint<'outer>, ErrorKind> {
        let ty = lhs.narrow(rhs)?;
        Ok(match op {
            BinaryOperator::Add => ty,
            BinaryOperator::Minus => ty,
            BinaryOperator::Mult => ty,
            BinaryOperator::And => self.from_type(&TypeKind::Bool).unwrap(),
            BinaryOperator::Cmp(_) => self.from_type(&TypeKind::Bool).unwrap(),
        })
    }
}
