use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DebugScopeValue(pub Vec<usize>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DebugLocationValue(pub DebugScopeValue, pub usize);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DebugMetadataValue(pub usize);

#[derive(Debug, Clone)]
pub struct DebugFileData {
    pub name: String,
    pub directory: String,
}

#[derive(Debug, Clone)]
pub(crate) struct DebugScopeHolder {
    value: DebugScopeValue,
    location: Option<DebugLocation>,
    inner_scopes: Vec<DebugScopeHolder>,
    locations: Vec<DebugLocationHolder>,
}

#[derive(Debug, Clone)]
pub struct DebugMetadataHolder {
    scope: DebugScopeValue,
    value: DebugMetadataValue,
    data: DebugMetadata,
}

#[derive(Debug, Clone)]
pub(crate) struct DebugLocationHolder {
    value: DebugLocationValue,
    location: DebugLocation,
}

#[derive(Debug, Clone)]
pub struct DebugLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone)]
pub struct DebugInformation {
    file: DebugFileData,
    scope: Rc<RefCell<DebugScopeHolder>>,
    metadata: Rc<RefCell<Vec<DebugMetadataHolder>>>,
}

impl DebugInformation {
    pub fn from_file(file: DebugFileData) -> (DebugInformation, DebugScopeValue) {
        let scope_value = DebugScopeValue(Vec::new());
        (
            DebugInformation {
                file,
                scope: Rc::new(RefCell::new(DebugScopeHolder {
                    value: scope_value.clone(),
                    inner_scopes: Vec::new(),
                    locations: Vec::new(),
                    location: None,
                })),
                metadata: Rc::new(RefCell::new(Vec::new())),
            },
            scope_value,
        )
    }

    pub fn inner_scope(
        &self,
        parent: &DebugScopeValue,
        location: DebugLocation,
    ) -> DebugScopeValue {
        unsafe {
            let mut outer_scope = RefMut::map(self.scope.borrow_mut(), |mut v| {
                for i in &parent.0 {
                    v = v.inner_scopes.get_unchecked_mut(*i);
                }
                v
            });

            let mut arr = parent.0.clone();
            arr.push(parent.0.len());
            let value = DebugScopeValue(arr);

            outer_scope.inner_scopes.push(DebugScopeHolder {
                value: value.clone(),
                inner_scopes: Vec::new(),
                locations: Vec::new(),
                location: Some(location),
            });
            value
        }
    }

    pub fn location(
        &self,
        scope_value: &DebugScopeValue,
        line: u32,
        column: u32,
    ) -> DebugLocationValue {
        unsafe {
            let mut scope = RefMut::map(self.scope.borrow_mut(), |mut v| {
                for i in &scope_value.0 {
                    v = v.inner_scopes.get_unchecked_mut(*i);
                }
                v
            });
            let value = DebugLocationValue(scope_value.clone(), scope.locations.len());
            let location = DebugLocationHolder {
                value: value.clone(),
                location: DebugLocation { line, column },
            };
            scope.locations.push(location);
            value
        }
    }

    pub fn metadata(&self, scope: &DebugScopeValue, kind: DebugMetadata) -> DebugMetadataValue {
        let mut metadata = self.metadata.borrow_mut();
        let value = DebugMetadataValue(metadata.len());
        metadata.push(DebugMetadataHolder {
            scope: scope.clone(),
            value: value.clone(),
            data: kind,
        });
        value
    }
}

#[derive(Debug, Clone)]
pub enum DebugMetadata {
    Function(DebugFunction),
    BasicType(DebugBasicType),
    ParamVar(DebugParamVariable),
    LocalVar(DebugLocalVariable),
}

#[derive(Debug, Clone)]
pub struct DebugBasicType {
    pub name: String,
    pub size_bits: u32,
    pub encoding: AteEncoding,
}

#[derive(Debug, Clone)]
pub enum AteEncoding {
    Address = 1,
    Boolean = 2,
    Float = 4,
    Signed = 5,
    SignedChar = 6,
    Unsigned = 7,
    UnsignedChar = 8,
}

#[derive(Debug, Clone)]
pub struct DebugFunction {
    pub scope: DebugScopeValue,
    pub name: String,
    pub linkage_name: String,
    pub location: DebugLocation,
    pub return_ty: DebugMetadataValue,
    pub is_local: bool,
    pub is_definition: bool,
    pub is_optimized: bool,
    pub scope_line: u32,
}

#[derive(Debug, Clone)]
pub struct DebugParamVariable {
    pub scope: DebugScopeValue,
    pub name: String,
    pub arg_idx: u32,
    pub location: DebugLocation,
    pub ty: DebugMetadataValue,
    pub always_preserve: bool,
}

#[derive(Debug, Clone)]
pub struct DebugLocalVariable {
    pub scope: DebugScopeValue,
    pub name: String,
    pub location: DebugLocation,
    pub ty: DebugMetadataValue,
    pub always_preserve: bool,
    pub alignment: u32,
}
