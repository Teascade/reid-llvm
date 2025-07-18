use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DebugScopeValue(pub Vec<usize>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct DebugLocationValue(pub DebugScopeValue, pub usize);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct DebugTypeValue(pub usize);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct DebugMetadataValue(pub usize);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct DebugSubprogramValue(pub usize);

#[derive(Debug, Clone)]
pub struct DebugFileData {
    pub name: String,
    pub directory: String,
}

#[derive(Debug, Clone)]
pub(crate) struct DebugScopeHolder {
    pub(crate) value: DebugScopeValue,
    pub(crate) location: Option<DebugLocation>,
    pub(crate) inner_scopes: Vec<DebugScopeHolder>,
    pub(crate) locations: Vec<DebugLocationHolder>,
}

#[derive(Debug, Clone)]
pub struct DebugMetadataHolder {
    pub(crate) scope: DebugScopeValue,
    pub(crate) value: DebugMetadataValue,
    pub(crate) data: DebugMetadata,
}

#[derive(Debug, Clone)]
pub struct DebugTypeHolder {
    pub(crate) value: DebugTypeValue,
    pub(crate) data: DebugTypeData,
}

#[derive(Debug, Clone)]
pub struct DebugSubprogramHolder {
    pub(crate) value: DebugSubprogramValue,
    pub(crate) data: DebugSubprogramData,
}

#[derive(Debug, Clone)]
pub(crate) struct DebugLocationHolder {
    value: DebugLocationValue,
    location: DebugLocation,
}

#[derive(Debug, Clone)]
pub struct DebugInformation {
    pub file: DebugFileData,
    scope: Rc<RefCell<DebugScopeHolder>>,
    subprograms: Rc<RefCell<Vec<DebugSubprogramHolder>>>,
    metadata: Rc<RefCell<Vec<DebugMetadataHolder>>>,
    types: Rc<RefCell<Vec<DebugTypeHolder>>>,
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
                subprograms: Rc::new(RefCell::new(Vec::new())),
                types: Rc::new(RefCell::new(Vec::new())),
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

    pub fn debug_type(&self, kind: DebugTypeData) -> DebugTypeValue {
        let mut types = self.types.borrow_mut();
        let value = DebugTypeValue(types.len());
        types.push(DebugTypeHolder {
            value: value.clone(),
            data: kind,
        });
        value
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

    fn check_metadata(&self, scope: &DebugScopeValue, metadata: &DebugMetadata) {
        match &metadata {
            DebugMetadata::ParamVar(debug_param_variable) => todo!(),
            DebugMetadata::LocalVar(debug_local_variable) => todo!(),
        }
    }

    pub fn subprogram(&self, kind: DebugSubprogramData) -> DebugSubprogramValue {
        let mut subprogram = self.subprograms.borrow_mut();
        let value = DebugSubprogramValue(subprogram.len());
        subprogram.push(DebugSubprogramHolder {
            value: value.clone(),
            data: kind,
        });
        value
    }

    pub fn get_metadata(&self) -> Rc<RefCell<Vec<DebugMetadataHolder>>> {
        self.metadata.clone()
    }

    pub fn get_scopes(&self) -> Rc<RefCell<DebugScopeHolder>> {
        self.scope.clone()
    }

    pub fn get_subprograms(&self) -> Rc<RefCell<Vec<DebugSubprogramHolder>>> {
        self.subprograms.clone()
    }

    pub fn get_types(&self) -> Rc<RefCell<Vec<DebugTypeHolder>>> {
        self.types.clone()
    }

    pub fn get_subprogram_data(&self, value: &DebugSubprogramValue) -> DebugSubprogramData {
        unsafe {
            self.subprograms
                .borrow()
                .get_unchecked(value.0)
                .data
                .clone()
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DebugLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone)]
pub enum DebugMetadata {
    ParamVar(DebugParamVariable),
    LocalVar(DebugLocalVariable),
}

#[derive(Debug, Clone)]
pub struct DebugParamVariable {
    pub name: String,
    /// the index (starting from 1) of this variable in the subprogram
    /// parameters. arg_idx should not conflict with other parameters of the
    /// same subprogram.
    pub arg_idx: u32,
    /// Used for line number
    pub location: DebugLocation,
    pub ty: DebugTypeValue,
    /// If this variable will be referenced from its containing subprogram, and
    /// will survive some optimizations.
    pub always_preserve: bool,
    pub flags: DwarfFlags,
}

#[derive(Debug, Clone)]
pub struct DebugLocalVariable {
    pub name: String,
    pub location: DebugLocation,
    pub ty: DebugMetadataValue,
    pub always_preserve: bool,
    pub alignment: u32,
}

impl Default for DebugSubprogramOptionals {
    fn default() -> Self {
        Self {
            scope_line: 0,
            is_local: false,
            is_definition: true,
            is_optimized: false,
            flags: DwarfFlags,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DwarfFlags;

#[derive(Debug, Clone)]
pub enum DebugTypeData {
    Basic(DebugBasicType),
    Subprogram(DebugSubprogramTypeData),
}

#[derive(Debug, Clone)]
pub struct DebugBasicType {
    pub name: String,
    /// Size of the type.
    pub size_bits: u64,
    /// DWARF encoding code, e.g., dwarf::DW_ATE_float.
    pub encoding: DwarfEncoding,
    /// Optional DWARF attributes, e.g., DW_AT_endianity.
    pub flags: DwarfFlags,
}

#[derive(Debug, Clone)]
pub struct DebugSubprogramTypeData {
    pub parameters: Vec<DebugTypeValue>,
    pub flags: DwarfFlags,
}

pub struct DebugSubprogramType {
    pub params: Vec<DebugSubprogramValue>,
    pub flags: DwarfFlags,
}

#[derive(Debug, Clone, Copy)]
pub enum DwarfEncoding {
    Address = 1,
    Boolean = 2,
    Float = 4,
    Signed = 5,
    SignedChar = 6,
    Unsigned = 7,
    UnsignedChar = 8,
}

#[derive(Debug, Clone)]
pub struct DebugSubprogramData {
    /// Function name.
    pub name: String,
    pub scope: DebugScopeValue,
    /// Used for line number.
    pub location: DebugLocation,
    /// Function type.
    pub ty: DebugTypeValue,
    pub opts: DebugSubprogramOptionals,
}

#[derive(Debug, Clone)]
pub struct DebugSubprogramOptionals {
    /// Set to the beginning of the scope this starts
    pub scope_line: u32,
    pub is_local: bool,
    pub is_definition: bool,
    pub is_optimized: bool,
    /// These flags are used to emit dwarf attributes. e.g. is this function
    /// prototyped or not.
    pub flags: DwarfFlags,
}
