use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    typecheck::{Collapsable, ErrorKind},
    BinaryOperator, Literal, TypeKind, VagueType,
};

#[derive(Clone)]
pub struct ScopeHint<'scope>(TypeIdRef, &'scope ScopeHints<'scope>);

impl<'scope> ScopeHint<'scope> {
    pub unsafe fn resolve_type(&self) -> TypeKind {
        unsafe { *self.1.types.hints.borrow().get_unchecked(*self.0.borrow()) }
    }

    pub fn narrow(&mut self, ty_ref: &TypeRef) -> Result<ScopeHint<'scope>, ErrorKind> {
        match ty_ref {
            TypeRef::Hint(other) => self.1.combine_vars(self, other),
            TypeRef::Literal(ty) => self.1.narrow_to_type(self, ty),
        }
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
        let inner_idx = self
            .types
            .type_refs
            .borrow()
            .get(idx)
            .map(|i| *i.borrow())?;
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

    fn new_vague(&'outer self, vague: &VagueType) -> ScopeHint<'outer> {
        let idx = self.types.new(TypeKind::Vague(*vague));
        ScopeHint(idx, self)
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
                if *idx == hint2.0 {
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
        lhs: &mut TypeRef<'outer>,
        rhs: &mut TypeRef<'outer>,
    ) -> Result<TypeRef<'outer>, ErrorKind> {
        let ty = lhs.narrow(rhs)?;
        Ok(match op {
            BinaryOperator::Add => ty,
            BinaryOperator::Minus => ty,
            BinaryOperator::Mult => ty,
            BinaryOperator::And => TypeRef::Literal(TypeKind::Bool),
            BinaryOperator::Cmp(_) => TypeRef::Literal(TypeKind::Bool),
        })
    }
}

#[derive(Debug)]
pub enum TypeRef<'scope> {
    Hint(ScopeHint<'scope>),
    Literal(TypeKind),
}

impl<'scope> TypeRef<'scope> {
    pub fn narrow(&mut self, other: &mut TypeRef<'scope>) -> Result<TypeRef<'scope>, ErrorKind> {
        match (self, other) {
            (TypeRef::Hint(hint), unk) | (unk, TypeRef::Hint(hint)) => {
                Ok(TypeRef::Hint(hint.narrow(unk)?))
            }
            (TypeRef::Literal(lit1), TypeRef::Literal(lit2)) => {
                Ok(TypeRef::Literal(lit1.collapse_into(lit2)?))
            }
        }
    }

    pub fn from_type(hints: &'scope ScopeHints<'scope>, ty: TypeKind) -> TypeRef<'scope> {
        match &ty.known() {
            Ok(ty) => TypeRef::Literal(*ty),
            Err(vague) => match &vague {
                super::VagueType::Hinted(idx) => TypeRef::Hint(ScopeHint(
                    unsafe { hints.types.type_refs.borrow().get_unchecked(*idx).clone() },
                    hints,
                )),
                _ => TypeRef::Hint(hints.new_vague(vague)),
            },
        }
    }

    pub fn from_literal(
        hints: &'scope ScopeHints<'scope>,
        lit: Literal,
    ) -> Result<TypeRef<'scope>, ErrorKind> {
        Ok(match lit {
            Literal::Vague(vague) => TypeRef::Hint(hints.new_vague(&vague.as_type())),
            _ => TypeRef::Literal(lit.as_type()),
        })
    }
}
