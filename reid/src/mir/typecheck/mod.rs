use std::collections::HashSet;

use crate::mir::VagueType as Vague;
use crate::mir::*;
use typecheck::ErrorTypedefKind;
use typerefs::TypeRefs;

use super::implement::{NumValueError, TypeCategory};
use super::pass::PassState;

pub mod typecheck;
pub mod typeinference;
pub mod typerefs;

pub type TypecheckPassState<'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

#[derive(thiserror::Error, Debug, Clone, PartialEq, PartialOrd)]
pub enum ErrorKind {
    #[error("NULL error, should never occur!")]
    Null,
    #[error("Type is vague: {0}")]
    TypeIsVague(VagueType),
    #[error("Literal {0} can not be coerced to type {1}")]
    LiteralIncompatible(Literal, TypeKind),
    #[error("Types {0} and {1} are incompatible")]
    TypesIncompatible(TypeKind, TypeKind),
    #[error("Variable not defined: {0}")]
    VariableNotDefined(String),
    #[error("Function {0} not defined")]
    FunctionNotDefined(String),
    #[error("Function {0} not defined for type {1}")]
    AssocFunctionNotDefined(String, TypeKind),
    #[error("Expected a return type of {0}, got {1} instead")]
    ReturnTypeMismatch(TypeKind, TypeKind),
    #[error("Function {0} already defined {1}")]
    FunctionAlreadyDefined(String, ErrorTypedefKind),
    #[error("Function {0}::{1} already defined {2}")]
    AssocFunctionAlreadyDefined(TypeKind, String, ErrorTypedefKind),
    #[error("Variable already defined: {0}")]
    VariableAlreadyDefined(String),
    #[error("Variable {0} is not declared as mutable")]
    VariableNotMutable(String),
    #[error("Function {0} was given {1} parameters, but {2} were expected")]
    InvalidAmountParameters(String, usize, usize),
    #[error("Unable to infer type {0}")]
    TypeNotInferrable(TypeKind),
    #[error("Expected branch type to be {0}, found {1} instead")]
    BranchTypesDiffer(TypeKind, TypeKind),
    #[error("Attempted to index a non-indexable type of {0}")]
    TriedIndexingNonIndexable(TypeKind),
    #[error("Index {0} out of bounds ({1})")]
    IndexOutOfBounds(u64, u64),
    #[error("No such type {0} could be found in module {1}")]
    NoSuchType(String, SourceModuleId),
    #[error("Attempted to access field of non-struct type of {0}")]
    TriedAccessingNonStruct(TypeKind),
    #[error("No such struct-field on type {0}")]
    NoSuchField(String),
    #[error("Missing definition for field \"{0}\"")]
    MissingStructField(String),
    #[error("Struct field declared twice {0}")]
    DuplicateStructField(String),
    #[error("Type declared twice {0}")]
    DuplicateTypeName(String),
    #[error("Recursive type definition: {0}.{1}")]
    RecursiveTypeDefinition(String, String),
    #[error("This type of expression can not be used for assignment")]
    InvalidSetExpression,
    #[error("Can not deref {0}, as it is not a borrow")]
    AttemptedDerefNonBorrow(TypeKind),
    #[error("Can not borrow this kind of expression")]
    ImpossibleBorrow,
    #[error("Types {0} and {1} differ in mutability")]
    TypesDifferMutability(TypeKind, TypeKind),
    #[error("Cannot mutably borrow variable {0}, which is not declared as mutable")]
    ImpossibleMutableBorrow(String),
    #[error("Cannot declare variable {0} as mutable, when it's type is immutable")]
    ImpossibleMutLet(String),
    #[error("Cannot produce a negative unsigned value of type {0}")]
    NegativeUnsignedValue(TypeKind),
    #[error("Cannot cast type {0} into type {1}")]
    NotCastableTo(TypeKind, TypeKind),
    #[error(transparent)]
    NumValueError(#[from] NumValueError),
    #[error("Binary operation {0} between {1} and {2} is already defined")]
    BinaryOpAlreadyDefined(BinaryOperator, TypeKind, TypeKind),
    #[error("Binary operation {0} between {1} and {2} is not defined")]
    InvalidBinop(BinaryOperator, TypeKind, TypeKind),
    #[error("Could not infer type for {0:?}. Try adding type annotations.")]
    CouldNotInferType(String),
    #[error("Arguments for a macro-function may only contain literals")]
    MacroMustBeLiterals,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HintKind {
    Coerce(TypeKind),
    Default,
    None,
}

impl HintKind {
    pub fn map<T>(&self, fun: T) -> HintKind
    where
        T: FnOnce(&TypeKind) -> TypeKind,
    {
        match self {
            HintKind::Coerce(type_kind) => HintKind::Coerce(fun(type_kind)),
            _ => self.clone(),
        }
    }
}

impl From<Option<TypeKind>> for HintKind {
    fn from(value: Option<TypeKind>) -> Self {
        match value {
            Some(ty) => HintKind::Coerce(ty),
            None => HintKind::None,
        }
    }
}

impl TypeKind {
    pub(super) fn narrow_into(&self, other: &TypeKind) -> Result<TypeKind, ErrorKind> {
        if self == other {
            return Ok(self.clone());
        }

        match (self, other) {
            (TypeKind::Vague(Vague::Integer), other) | (other, TypeKind::Vague(Vague::Integer)) => match other {
                TypeKind::Vague(Vague::Unknown) => Ok(TypeKind::Vague(Vague::Integer)),
                TypeKind::Vague(Vague::Integer) => Ok(TypeKind::Vague(Vague::Integer)),
                TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::I128
                | TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::U128
                | TypeKind::F16
                | TypeKind::F32B
                | TypeKind::F32
                | TypeKind::F64
                | TypeKind::F80
                | TypeKind::F128
                | TypeKind::F128PPC => Ok(other.clone()),
                _ => Err(ErrorKind::TypesIncompatible(self.clone(), other.clone())),
            },
            (TypeKind::Vague(Vague::Decimal), other) | (other, TypeKind::Vague(Vague::Decimal)) => match other {
                TypeKind::Vague(Vague::Unknown) => Ok(TypeKind::Vague(Vague::Decimal)),
                TypeKind::Vague(Vague::Decimal) => Ok(TypeKind::Vague(Vague::Decimal)),
                TypeKind::F16
                | TypeKind::F32B
                | TypeKind::F32
                | TypeKind::F64
                | TypeKind::F80
                | TypeKind::F128
                | TypeKind::F128PPC => Ok(other.clone()),
                _ => Err(ErrorKind::TypesIncompatible(self.clone(), other.clone())),
            },
            (TypeKind::Vague(Vague::Unknown), other) | (other, TypeKind::Vague(Vague::Unknown)) => Ok(other.clone()),
            (TypeKind::Borrow(val1, mut1), TypeKind::Borrow(val2, mut2)) => {
                // Extracted to give priority for other collapse-error
                let collapsed = val1.narrow_into(val2)?;
                if mut1 == mut2 || (*mut1 && !mut2) {
                    Ok(TypeKind::Borrow(Box::new(collapsed), *mut1 && *mut2))
                } else {
                    Err(ErrorKind::TypesDifferMutability(self.clone(), other.clone()))
                }
            }
            (TypeKind::UserPtr(val1), TypeKind::UserPtr(val2)) => {
                Ok(TypeKind::UserPtr(Box::new(val1.narrow_into(val2)?)))
            }
            (TypeKind::Array(val1, len1), TypeKind::Array(val2, len2)) => {
                if len1 != len2 {
                    return Err(ErrorKind::TypesIncompatible(self.clone(), other.clone()));
                }
                Ok(TypeKind::Array(Box::new(val1.narrow_into(val2)?), *len1))
            }
            _ => Err(ErrorKind::TypesIncompatible(self.clone(), other.clone())),
        }
    }

    pub(super) fn widen_into(&self, other: &TypeKind) -> TypeKind {
        if self == other {
            return self.clone();
        }
        match (self, other) {
            (TypeKind::Vague(Vague::Unknown), _) | (_, TypeKind::Vague(Vague::Unknown)) => {
                TypeKind::Vague(VagueType::Unknown)
            }
            (TypeKind::Vague(Vague::Integer), other) | (other, TypeKind::Vague(Vague::Integer)) => match other {
                TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::I128
                | TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::U128 => TypeKind::Vague(VagueType::Integer),
                _ => TypeKind::Vague(VagueType::Unknown),
            },
            (TypeKind::Vague(Vague::Decimal), other) | (other, TypeKind::Vague(Vague::Decimal)) => match other {
                TypeKind::F16
                | TypeKind::F32B
                | TypeKind::F32
                | TypeKind::F64
                | TypeKind::F80
                | TypeKind::F128
                | TypeKind::F128PPC => TypeKind::Vague(VagueType::Decimal),
                _ => TypeKind::Vague(VagueType::Unknown),
            },
            (TypeKind::UserPtr(val1), TypeKind::UserPtr(val2)) => TypeKind::UserPtr(Box::new(val1.widen_into(val2))),
            (TypeKind::CodegenPtr(val1), TypeKind::CodegenPtr(val2)) => {
                TypeKind::CodegenPtr(Box::new(val1.widen_into(val2)))
            }
            (TypeKind::Array(val1, len1), TypeKind::Array(val2, len2)) => {
                if len1 == len2 {
                    TypeKind::Array(Box::new(val1.widen_into(val2)), *len1)
                } else {
                    TypeKind::Vague(VagueType::Unknown)
                }
            }
            (TypeKind::Borrow(val1, mutable1), TypeKind::Borrow(val2, mutable2)) => {
                if mutable1 == mutable2 {
                    TypeKind::Borrow(Box::new(val1.widen_into(val2)), *mutable1)
                } else {
                    TypeKind::Vague(VagueType::Unknown)
                }
            }
            _ => {
                if self.category() == other.category() {
                    match self.category() {
                        TypeCategory::Integer => TypeKind::Vague(VagueType::Integer),
                        TypeCategory::Real => TypeKind::Vague(VagueType::Decimal),
                        TypeCategory::Bool => TypeKind::Bool,
                        _ => TypeKind::Vague(VagueType::Unknown),
                    }
                } else {
                    TypeKind::Vague(VagueType::Unknown)
                }
            }
        }
    }

    pub(super) fn cast_into(&self, other: &TypeKind) -> Result<TypeKind, ErrorKind> {
        if let Ok(collapsed) = self.narrow_into(other) {
            Ok(collapsed)
        } else {
            let self_cat = self.category();
            let other_cat = other.category();
            match (self, other) {
                (TypeKind::UserPtr(_), TypeKind::UserPtr(_)) => Ok(other.clone()),
                (TypeKind::Borrow(ty1, _), TypeKind::UserPtr(ty2)) => match *ty1.clone() {
                    TypeKind::Array(ty1, _) => {
                        if ty1 == *ty2 {
                            Ok(other.clone())
                        } else {
                            Err(ErrorKind::NotCastableTo(self.clone(), other.clone()))
                        }
                    }
                    _ => Err(ErrorKind::NotCastableTo(self.clone(), other.clone())),
                },
                (TypeKind::Char, TypeKind::U8) => Ok(other.clone()),
                (TypeKind::U8, TypeKind::Char) => Ok(other.clone()),
                _ => match (&self_cat, &other_cat) {
                    (TypeCategory::Integer, TypeCategory::Integer) => Ok(other.clone()),
                    (TypeCategory::Integer, TypeCategory::Real) => Ok(other.clone()),
                    (TypeCategory::Real, TypeCategory::Integer) => Ok(other.clone()),
                    (TypeCategory::Real, TypeCategory::Real) => Ok(other.clone()),
                    _ => Err(ErrorKind::NotCastableTo(self.clone(), other.clone())),
                },
            }
        }
    }

    /// Assert that a type is already known and not vague. Return said type or
    /// error.
    pub(super) fn assert_unvague(&self) -> Result<TypeKind, ErrorKind> {
        self.known().map_err(ErrorKind::TypeIsVague)
    }

    /// Try to collapse a type on itself producing a default type if one exists,
    /// Error if not.
    pub(super) fn or_default(&self) -> Result<TypeKind, ErrorKind> {
        Ok(match self {
            TypeKind::Vague(vague_type) => match &vague_type {
                Vague::Unknown => Err(ErrorKind::TypeIsVague(*vague_type))?,
                Vague::Integer => TypeKind::I32,
                Vague::TypeRef(_) => panic!("Hinted default!"),
                VagueType::Decimal => TypeKind::F32,
            },
            TypeKind::Array(type_kind, len) => TypeKind::Array(Box::new(type_kind.or_default()?), *len),
            TypeKind::Borrow(type_kind, mutable) => TypeKind::Borrow(Box::new(type_kind.or_default()?), *mutable),
            TypeKind::UserPtr(type_kind) => TypeKind::UserPtr(Box::new(type_kind.or_default()?)),
            TypeKind::CodegenPtr(type_kind) => TypeKind::CodegenPtr(Box::new(type_kind.or_default()?)),
            _ => self.clone(),
        })
    }

    pub(super) fn resolve_weak(&self, refs: &TypeRefs) -> TypeKind {
        match self {
            TypeKind::Vague(Vague::TypeRef(idx)) => refs.retrieve_wide_type(*idx, &mut HashSet::new()).unwrap(),
            _ => self.clone(),
        }
    }

    pub(super) fn resolve_ref(&self, refs: &TypeRefs) -> TypeKind {
        let resolved = self.resolve_weak(refs);
        match resolved {
            TypeKind::Array(t, len) => TypeKind::Array(Box::new(t.resolve_ref(refs)), len),
            TypeKind::Borrow(inner, mutable) => TypeKind::Borrow(Box::new(inner.resolve_ref(refs)), mutable),
            _ => resolved,
        }
    }

    pub(super) fn assert_known(&self, state: &TypecheckPassState) -> Result<TypeKind, ErrorKind> {
        self.is_known(state).map(|_| self.clone())
    }

    pub(super) fn is_known(&self, state: &TypecheckPassState) -> Result<(), ErrorKind> {
        match &self {
            TypeKind::Array(type_kind, _) => type_kind.as_ref().is_known(state),
            TypeKind::CustomType(custom_type_key) => {
                state
                    .scope
                    .get_type(custom_type_key)
                    .map(|_| ())
                    .ok_or(ErrorKind::NoSuchType(
                        custom_type_key.0.clone(),
                        custom_type_key.1.clone(),
                    ))
            }
            TypeKind::Borrow(type_kind, _) => type_kind.is_known(state),
            TypeKind::UserPtr(type_kind) => type_kind.is_known(state),
            TypeKind::CodegenPtr(type_kind) => type_kind.is_known(state),
            TypeKind::Vague(vague_type) => Err(ErrorKind::TypeIsVague(*vague_type)),
            _ => Ok(()),
        }
    }
}
