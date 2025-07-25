use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    ast::BinopDefinition,
    mir::{pass::BinopMap, BinaryOperator, TypeKind, VagueType},
};

use super::{
    super::pass::{ScopeBinopDef, ScopeBinopKey, Storage},
    ErrorKind,
};

#[derive(Clone)]
pub struct TypeRef<'scope>(pub(super) TypeIdRef, pub(super) &'scope ScopeTypeRefs<'scope>);

impl<'scope> TypeRef<'scope> {
    /// Resolve current type in a weak manner, not resolving any Arrays or
    /// further inner types
    pub fn resolve_weak(&self) -> Option<TypeKind> {
        Some(self.1.types.retrieve_wide_type(*self.0.borrow())?)
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
        TypeKind::Vague(VagueType::TypeRef(*self.0.borrow()))
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

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum TypeRefKind {
    Direct(TypeKind),
    BinOp(BinaryOperator, TypeKind, TypeKind),
}

impl TypeRefKind {
    pub fn widen(&self, types: &TypeRefs) -> TypeKind {
        match self {
            TypeRefKind::BinOp(op, lhs, rhs) => {
                let mut binops = types
                    .binop_types
                    .iter()
                    .filter(|b| b.1.operator == *op)
                    .map(|b| {
                        b.1.narrow(&lhs.resolve_ref(types), &rhs.resolve_ref(types))
                            .map(|b| b.2)
                    })
                    .filter_map(|s| s);
                if let Some(mut ty) = binops.next() {
                    while let Some(other) = binops.next() {
                        ty = ty.widen_into(&other);
                    }
                    ty
                } else {
                    TypeKind::Vague(VagueType::Unknown)
                }
            }
            TypeRefKind::Direct(ty) => match ty {
                TypeKind::Vague(VagueType::TypeRef(id)) => types.retrieve_wide_type(*id).unwrap(),
                _ => ty.clone(),
            },
        }
    }
}

#[derive(Default, Debug)]
pub struct TypeRefs {
    /// Simple list of types that variables can refrence
    pub(super) hints: RefCell<Vec<TypeRefKind>>,
    /// Indirect ID-references, referring to hints-vec
    pub(super) type_refs: RefCell<Vec<TypeIdRef>>,
    pub(super) binop_types: BinopMap,
}

impl std::fmt::Display for TypeRefs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, typeref) in self.type_refs.borrow().iter().enumerate() {
            let idx = *typeref.borrow();
            writeln!(
                f,
                "{:<3} = {:<3} = {:?} = {:?}",
                i,
                unsafe { *self.recurse_type_ref(idx).borrow() },
                self.retrieve_typeref(idx),
                self.retrieve_wide_type(idx),
            )?;
        }
        Ok(())
    }
}

impl TypeRefs {
    pub fn with_binops(binops: BinopMap) -> TypeRefs {
        TypeRefs {
            hints: Default::default(),
            type_refs: Default::default(),
            binop_types: binops,
        }
    }

    pub fn new(&self, ty: &TypeKind) -> TypeIdRef {
        let idx = self.hints.borrow().len();
        let typecell = Rc::new(RefCell::new(idx));
        self.type_refs.borrow_mut().push(typecell.clone());
        self.hints.borrow_mut().push(TypeRefKind::Direct(ty.clone()));
        typecell
    }

    pub fn binop(&self, op: BinaryOperator, lhs: &TypeRef, rhs: &TypeRef) -> TypeIdRef {
        let idx = self.hints.borrow().len();
        let typecell = Rc::new(RefCell::new(idx));
        self.type_refs.borrow_mut().push(typecell.clone());
        self.hints
            .borrow_mut()
            .push(TypeRefKind::BinOp(op, lhs.as_type(), rhs.as_type()));
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
            .find(|(_, t)| t == &&TypeRefKind::Direct(ty.clone()))
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

    pub fn retrieve_typeref(&self, idx: usize) -> Option<TypeRefKind> {
        let inner_idx = unsafe { *self.recurse_type_ref(idx).borrow() };
        self.hints.borrow().get(inner_idx).cloned()
    }

    pub fn retrieve_wide_type(&self, idx: usize) -> Option<TypeKind> {
        self.retrieve_typeref(idx).map(|t| t.widen(self))
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
        self.variables.borrow_mut().insert(name, (mutable, type_ref.0.clone()));
        Ok(type_ref)
    }

    pub fn from_type(&'outer self, ty: &TypeKind) -> Option<TypeRef<'outer>> {
        let idx = match ty {
            TypeKind::Vague(VagueType::TypeRef(idx)) => {
                let inner_idx = unsafe { *self.types.recurse_type_ref(*idx).borrow() };
                self.types.type_refs.borrow().get(inner_idx).cloned()?
            }
            TypeKind::Vague(_) => self.types.new(ty),
            TypeKind::Array(elem_ty, length) => {
                let elem_ty = self.from_type(elem_ty)?;
                self.types.new(&TypeKind::Array(Box::new(elem_ty.as_type()), *length))
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

    pub fn from_binop(&'outer self, op: BinaryOperator, lhs: &TypeRef, rhs: &TypeRef) -> TypeRef<'outer> {
        TypeRef(self.types.binop(op, lhs, rhs), self)
    }

    fn narrow_to_type(&'outer self, hint: &TypeRef, ty: &TypeKind) -> Option<TypeRef<'outer>> {
        unsafe {
            let mut hints = self.types.hints.borrow_mut();
            let existing = hints.get_unchecked_mut(*hint.0.borrow());

            match existing {
                TypeRefKind::Direct(type_kind) => {
                    *type_kind = type_kind.narrow_into(&ty).ok()?;
                }
                TypeRefKind::BinOp(op, lhs, rhs) => {
                    let op = op.clone();
                    let lhs = lhs.clone();
                    let rhs = rhs.clone();
                    drop(hints);

                    let lhs_resolved = lhs.resolve_ref(self.types);
                    let rhs_resolved = rhs.resolve_ref(self.types);

                    let binops = self
                        .types
                        .binop_types
                        .iter()
                        .filter(|b| b.1.operator == op && b.1.return_ty == *ty)
                        .collect::<Vec<_>>();
                    for binop in binops {
                        if let (Ok(lhs_narrow), Ok(rhs_narrow)) = (
                            lhs_resolved.narrow_into(&binop.1.hands.0),
                            rhs_resolved.narrow_into(&binop.1.hands.1),
                        ) {
                            match &lhs {
                                TypeKind::Vague(VagueType::TypeRef(idx)) => {
                                    let mut lhs_ref = TypeRef(Rc::new(RefCell::new(*idx)), self);
                                    let narrowed = self.narrow_to_type(&mut lhs_ref, &lhs_narrow).unwrap_or(lhs_ref);
                                }
                                _ => {}
                            };
                            match &rhs {
                                TypeKind::Vague(VagueType::TypeRef(idx)) => {
                                    let mut rhs_ref = TypeRef(Rc::new(RefCell::new(*idx)), self);
                                    let narrowed = self.narrow_to_type(&mut rhs_ref, &rhs_narrow).unwrap_or(rhs_ref);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
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
                .clone()
                .widen(self.types);
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

    pub fn available_binops(
        &'outer self,
        op: &BinaryOperator,
        lhs: &mut TypeRef<'outer>,
        rhs: &mut TypeRef<'outer>,
    ) -> Vec<ScopeBinopDef> {
        let mut applying_binops = Vec::new();
        for (_, binop) in self.types.binop_types.iter() {
            if binop.operator != *op {
                continue;
            }
            if let Some(_) = check_binop(lhs, rhs, binop) {
                applying_binops.push(binop.clone());
                continue;
            }
            if binop.operator.is_commutative() {
                if let Some(_) = check_binop(lhs, rhs, binop) {
                    applying_binops.push(binop.clone());
                    continue;
                }
            }
        }
        applying_binops
    }
}

fn check_binop<'o>(
    lhs: &mut TypeRef<'o>,
    rhs: &mut TypeRef<'o>,
    binop: &ScopeBinopDef,
) -> Option<(TypeKind, TypeKind, TypeKind)> {
    binop.narrow(&lhs.resolve_deep()?, &rhs.resolve_deep()?)
}
