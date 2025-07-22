use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::mir::VagueType;

use super::{typecheck::ErrorKind, BinaryOperator, TypeKind};

#[derive(Clone)]
pub struct TypeRef<'scope>(
    pub(super) TypeIdRef,
    pub(super) &'scope ScopeTypeRefs<'scope>,
);

impl<'scope> TypeRef<'scope> {
    /// Resolve current type in a weak manner, not resolving any Arrays or
    /// further inner types
    pub fn resolve_weak(&self) -> Option<TypeKind> {
        Some(self.1.types.retrieve_type(*self.0.borrow())?)
    }

    /// Resolve type deeply, trying to resolve any inner types as well.
    pub fn resolve_deep(&self) -> Option<TypeKind> {
        let resolved = self.resolve_weak()?;

        match resolved {
            TypeKind::Array(elem_ty, len) => {
                let resolved_elem_ty = self.1.from_type(&elem_ty).unwrap().resolve_weak()?;
                Some(TypeKind::Array(Box::new(resolved_elem_ty), len))
            }
            _ => Some(resolved),
        }
    }

    pub fn narrow(&mut self, other: &TypeRef) -> Option<TypeRef<'scope>> {
        self.1.combine_vars(self, other)
    }

    pub fn as_type(&self) -> TypeKind {
        TypeKind::Vague(super::VagueType::TypeRef(*self.0.borrow()))
    }
}

impl<'scope> std::fmt::Debug for TypeRef<'scope> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Hint")
            .field(&self.0)
            .field(&self.resolve_deep().unwrap())
            .finish()
    }
}

type TypeIdRef = Rc<RefCell<usize>>;

#[derive(Debug, Default)]
pub struct TypeRefs {
    /// Simple list of types that variables can refrence
    pub(super) hints: RefCell<Vec<TypeKind>>,
    /// Indirect ID-references, referring to hints-vec
    pub(super) type_refs: RefCell<Vec<TypeIdRef>>,
}

impl TypeRefs {
    pub fn new(&self, ty: &TypeKind) -> TypeIdRef {
        let idx = self.hints.borrow().len();
        let typecell = Rc::new(RefCell::new(idx));
        self.type_refs.borrow_mut().push(typecell.clone());
        self.hints.borrow_mut().push(ty.clone());
        typecell
    }

    pub fn find(&self, ty: &TypeKind) -> Option<TypeIdRef> {
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
            .find(|(_, t)| *t == ty)
            .map(|(i, _)| i)
        {
            Some(Rc::new(RefCell::new(idx)))
        } else {
            None
        }
    }

    pub(super) unsafe fn recurse_type_ref(&self, mut idx: usize) -> TypeIdRef {
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

    pub fn retrieve_type(&self, idx: usize) -> Option<TypeKind> {
        let inner_idx = unsafe { *self.recurse_type_ref(idx).borrow() };
        self.hints
            .borrow()
            .get(inner_idx)
            .cloned()
            .map(|t| match t {
                TypeKind::Vague(VagueType::TypeRef(id)) => self.retrieve_type(id),
                _ => Some(t),
            })
            .flatten()
    }
}

#[derive(Debug)]
pub struct ScopeTypeRefs<'outer> {
    pub types: &'outer TypeRefs,
    outer: Option<&'outer ScopeTypeRefs<'outer>>,
    /// Mapping of what types variables point to
    variables: RefCell<HashMap<String, (bool, TypeIdRef)>>,
}

impl<'outer> ScopeTypeRefs<'outer> {
    pub fn from(types: &'outer TypeRefs) -> ScopeTypeRefs<'outer> {
        ScopeTypeRefs {
            types,
            outer: Default::default(),
            variables: Default::default(),
        }
    }

    pub fn new_var(
        &'outer self,
        name: String,
        mutable: bool,
        initial_ty: &TypeKind,
    ) -> Result<TypeRef<'outer>, ErrorKind> {
        if self.variables.borrow().contains_key(&name) {
            return Err(ErrorKind::VariableAlreadyDefined(name));
        }
        let type_ref = self.from_type(&initial_ty).unwrap();
        self.variables
            .borrow_mut()
            .insert(name, (mutable, type_ref.0.clone()));
        Ok(type_ref)
    }

    pub fn from_type(&'outer self, ty: &TypeKind) -> Option<TypeRef<'outer>> {
        let idx = match ty {
            TypeKind::Vague(super::VagueType::TypeRef(idx)) => {
                let inner_idx = unsafe { *self.types.recurse_type_ref(*idx).borrow() };
                self.types.type_refs.borrow().get(inner_idx).cloned()?
            }
            TypeKind::Vague(_) => self.types.new(ty),
            TypeKind::Array(elem_ty, length) => {
                let elem_ty = self.from_type(elem_ty)?;
                self.types
                    .new(&TypeKind::Array(Box::new(elem_ty.as_type()), *length))
            }
            TypeKind::Borrow(ty, mutable) => {
                let inner_ty = self.from_type(ty)?;
                self.types
                    .new(&&TypeKind::Borrow(Box::new(inner_ty.as_type()), *mutable))
            }
            _ => {
                if let Some(ty_ref) = self.types.find(ty) {
                    ty_ref
                } else {
                    self.types.new(ty)
                }
            }
        };
        Some(TypeRef(idx, self))
    }

    fn narrow_to_type(&'outer self, hint: &TypeRef, ty: &TypeKind) -> Option<TypeRef<'outer>> {
        unsafe {
            let mut hints = self.types.hints.borrow_mut();
            let existing = hints.get_unchecked_mut(*hint.0.borrow());
            *existing = existing.collapse_into(&ty).ok()?;
            Some(TypeRef(hint.0.clone(), self))
        }
    }

    fn combine_vars(&'outer self, hint1: &TypeRef, hint2: &TypeRef) -> Option<TypeRef<'outer>> {
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
            Some(TypeRef(hint1.0.clone(), self))
        }
    }

    pub fn inner(&'outer self) -> ScopeTypeRefs<'outer> {
        ScopeTypeRefs {
            types: self.types,
            outer: Some(self),
            variables: Default::default(),
        }
    }

    pub fn find_var(&'outer self, name: &String) -> Option<(bool, TypeRef<'outer>)> {
        self.variables
            .borrow()
            .get(name)
            .map(|(mutable, idx)| (*mutable, TypeRef(idx.clone(), self)))
            .or(self.outer.map(|o| o.find_var(name)).flatten())
    }

    pub fn binop(
        &'outer self,
        op: &BinaryOperator,
        lhs: &mut TypeRef<'outer>,
        rhs: &mut TypeRef<'outer>,
    ) -> Option<TypeRef<'outer>> {
        let ty = lhs.narrow(rhs)?;
        self.from_type(&ty.as_type().binop_type(op))
    }
}
