use std::{cell::RefCell, rc::Rc};

use crate::builder::InstructionValue;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct DebugScopeValue(pub Vec<usize>);

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct DebugLocationValue(pub DebugProgramValue, pub usize);

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct DebugTypeValue(pub usize);

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct DebugMetadataValue(pub usize);

/// Represents either a subprogram, or the compilation context
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct DebugProgramValue(pub usize);

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
    pub(crate) program: DebugProgramValue,
    pub(crate) value: DebugMetadataValue,
    pub(crate) data: DebugMetadata,
}

#[derive(Clone)]
pub struct DebugTypeHolder {
    pub(crate) value: DebugTypeValue,
    pub(crate) data: DebugTypeData,
}

#[derive(Debug, Clone)]
pub struct DebugSubprogramHolder {
    pub(crate) _value: DebugProgramValue,
    pub(crate) data: DebugSubprogramData,
}

#[derive(Debug, Clone)]
pub(crate) struct DebugLocationHolder {
    pub(crate) program: DebugProgramValue,
    pub(crate) value: DebugLocationValue,
    pub(crate) location: DebugLocation,
}

#[derive(Debug, Clone)]
pub struct DebugInformation {
    pub file: DebugFileData,
    // scope: Rc<RefCell<DebugScopeHolder>>,
    locations: Rc<RefCell<Vec<DebugLocationHolder>>>,
    programs: Rc<RefCell<Vec<DebugSubprogramHolder>>>,
    metadata: Rc<RefCell<Vec<DebugMetadataHolder>>>,
    types: Rc<RefCell<Vec<DebugTypeHolder>>>,
}

impl DebugInformation {
    pub fn from_file(file: DebugFileData) -> (DebugInformation, DebugProgramValue) {
        (
            DebugInformation {
                file,
                // scope: Rc::new(RefCell::new(DebugScopeHolder {
                //     value: scope_value.clone(),
                //     inner_scopes: Vec::new(),
                //     locations: Vec::new(),
                //     location: None,
                // })),
                locations: Rc::new(RefCell::new(Vec::new())),
                metadata: Rc::new(RefCell::new(Vec::new())),
                programs: Rc::new(RefCell::new(Vec::new())),
                types: Rc::new(RefCell::new(Vec::new())),
            },
            DebugProgramValue(0),
        )
    }

    // pub fn inner_scope(
    //     &self,
    //     parent: &DebugScopeValue,
    //     location: DebugLocation,
    // ) -> DebugScopeValue {
    //     unsafe {
    //         let mut outer_scope = RefMut::map(self.scope.borrow_mut(), |mut v| {
    //             for i in &parent.0 {
    //                 v = v.inner_scopes.get_unchecked_mut(*i);
    //             }
    //             v
    //         });

    //         let mut arr = parent.0.clone();
    //         arr.push(parent.0.len());
    //         let value = DebugScopeValue(arr);

    //         outer_scope.inner_scopes.push(DebugScopeHolder {
    //             value: value.clone(),
    //             inner_scopes: Vec::new(),
    //             locations: Vec::new(),
    //             location: Some(location),
    //         });
    //         value
    //     }
    // }

    pub fn location(
        &self,
        program_value: &DebugProgramValue,
        location: DebugLocation,
    ) -> DebugLocationValue {
        let value = DebugLocationValue(program_value.clone(), self.locations.borrow().len());
        let location = DebugLocationHolder {
            program: *program_value,
            value: value.clone(),
            location,
        };
        self.locations.borrow_mut().push(location);
        value
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

    pub fn metadata(&self, program: &DebugProgramValue, kind: DebugMetadata) -> DebugMetadataValue {
        let mut metadata = self.metadata.borrow_mut();
        let value = DebugMetadataValue(metadata.len());
        metadata.push(DebugMetadataHolder {
            program: *program,
            value: value.clone(),
            data: kind,
        });
        value
    }

    pub fn subprogram(&self, kind: DebugSubprogramData) -> DebugProgramValue {
        let mut subprogram = self.programs.borrow_mut();
        let value = DebugProgramValue(subprogram.len() + 1);
        subprogram.push(DebugSubprogramHolder {
            _value: value.clone(),
            data: kind,
        });
        value
    }

    pub fn get_metadata(&self) -> Rc<RefCell<Vec<DebugMetadataHolder>>> {
        self.metadata.clone()
    }

    // pub fn get_scopes(&self) -> Rc<RefCell<DebugScopeHolder>> {
    //     self.scope.clone()
    // }

    pub fn get_subprograms(&self) -> Rc<RefCell<Vec<DebugSubprogramHolder>>> {
        self.programs.clone()
    }

    pub fn get_types(&self) -> Rc<RefCell<Vec<DebugTypeHolder>>> {
        self.types.clone()
    }

    pub(crate) fn get_locations(&self) -> Rc<RefCell<Vec<DebugLocationHolder>>> {
        self.locations.clone()
    }

    pub fn get_subprogram_data(&self, value: &DebugProgramValue) -> DebugSubprogramData {
        unsafe {
            self.programs
                .borrow()
                .get_unchecked(value.0 - 1)
                .data
                .clone()
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct DebugLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone)]
pub enum DebugMetadata {
    ParamVar(DebugParamVariable),
    LocalVar(DebugLocalVariable),
    VarAssignment,
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
    pub ty: DebugTypeValue,
    pub always_preserve: bool,
    pub flags: DwarfFlags,
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

#[derive(Clone)]
pub enum DebugTypeData {
    Basic(DebugBasicType),
    Subprogram(DebugSubprogramType),
    Pointer(DebugPointerType),
    Array(DebugArrayType),
    Struct(DebugStructType),
}

#[derive(Clone)]
pub struct DebugBasicType {
    pub name: String,
    /// Size of the type.
    pub size_bits: u64,
    /// DWARF encoding code, e.g., dwarf::DW_ATE_float.
    pub encoding: DwarfEncoding,
    /// Optional DWARF attributes, e.g., DW_AT_endianity.
    pub flags: DwarfFlags,
}

#[derive(Clone)]
pub struct DebugArrayType {
    pub size_bits: u64,
    pub align_bits: u32,
    pub element_type: DebugTypeValue,
    pub length: u64,
}

#[derive(Clone)]
pub struct DebugPointerType {
    pub name: String,
    pub pointee: DebugTypeValue,
    pub size_bits: u64,
}
#[derive(Clone)]
pub struct DebugStructType {
    pub name: String,
    pub scope: DebugProgramValue,
    pub location: DebugLocation,
    pub size_bits: u64,
    pub flags: DwarfFlags,
    pub fields: Vec<DebugFieldType>,
}

#[derive(Clone)]
pub struct DebugFieldType {
    pub name: String,
    pub location: DebugLocation,
    pub size_bits: u64,
    pub offset: u64,
    pub flags: DwarfFlags,
    pub ty: DebugTypeValue,
}

#[derive(Clone)]
pub struct DebugSubprogramType {
    pub parameters: Vec<DebugTypeValue>,
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
    pub outer_scope: DebugProgramValue,
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

#[derive(Clone)]
pub struct InstructionDebugRecordData {
    pub scope: DebugProgramValue,
    pub variable: DebugMetadataValue,
    pub location: DebugLocation,
    pub kind: DebugRecordKind,
}

#[derive(Clone, Copy)]
pub enum DebugRecordKind {
    Declare(InstructionValue),
    Value(InstructionValue),
}
