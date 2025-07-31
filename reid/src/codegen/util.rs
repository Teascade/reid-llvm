use std::collections::HashMap;

use reid_lib::{
    builder::{InstructionValue, TypeValue},
    debug_information::{
        DebugArrayType, DebugBasicType, DebugFieldType, DebugInformation, DebugLocation, DebugPointerType,
        DebugPosition, DebugScopeValue, DebugStructType, DebugTypeData, DebugTypeValue, DwarfEncoding, DwarfFlags,
    },
    Block, CmpPredicate, ConstValueKind, Instr, Type,
};

use crate::{
    lexer::{FullToken, Position},
    mir::{self, CustomTypeKey, Metadata, SourceModuleId, TypeDefinition, TypeDefinitionKind, TypeKind, VagueLiteral},
};

use super::{
    scope::{Debug, Scope},
    ModuleCodegen,
};

impl mir::CmpOperator {
    pub(super) fn predicate(&self) -> CmpPredicate {
        match self {
            mir::CmpOperator::LT => CmpPredicate::LT,
            mir::CmpOperator::GT => CmpPredicate::GT,
            mir::CmpOperator::LE => CmpPredicate::LE,
            mir::CmpOperator::GE => CmpPredicate::GE,
            mir::CmpOperator::EQ => CmpPredicate::EQ,
            mir::CmpOperator::NE => CmpPredicate::NE,
        }
    }
}

impl mir::Literal {
    pub(super) fn as_const(&self, block: &mut Block) -> InstructionValue {
        block
            .build_named(format!("{}", self), Instr::Constant(self.as_const_kind()))
            .unwrap()
    }

    pub(super) fn as_const_kind(&self) -> ConstValueKind {
        match self.clone() {
            mir::Literal::I8(val) => ConstValueKind::I8(val),
            mir::Literal::I16(val) => ConstValueKind::I16(val),
            mir::Literal::I32(val) => ConstValueKind::I32(val),
            mir::Literal::I64(val) => ConstValueKind::I64(val),
            mir::Literal::I128(val) => ConstValueKind::I128(val),
            mir::Literal::U8(val) => ConstValueKind::U8(val),
            mir::Literal::U16(val) => ConstValueKind::U16(val),
            mir::Literal::U32(val) => ConstValueKind::U32(val),
            mir::Literal::U64(val) => ConstValueKind::U64(val),
            mir::Literal::U128(val) => ConstValueKind::U128(val),
            mir::Literal::Bool(val) => ConstValueKind::Bool(val),
            mir::Literal::String(val) => ConstValueKind::Str(val.clone()),
            mir::Literal::Vague(VagueLiteral::Number(val)) => ConstValueKind::I32(val as i32),
            mir::Literal::Vague(VagueLiteral::Decimal(val)) => ConstValueKind::F32(val as f32),
            mir::Literal::F16(val) => ConstValueKind::F16(val),
            mir::Literal::F32B(val) => ConstValueKind::F32B(val),
            mir::Literal::F32(val) => ConstValueKind::F32(val),
            mir::Literal::F64(val) => ConstValueKind::F64(val),
            mir::Literal::F80(val) => ConstValueKind::F80(val),
            mir::Literal::F128(val) => ConstValueKind::F128(val),
            mir::Literal::F128PPC(val) => ConstValueKind::F128PPC(val),
            mir::Literal::Char(c) => ConstValueKind::U8(c as u8),
        }
    }
}

impl TypeKind {
    pub(super) fn get_type(&self, type_vals: &HashMap<CustomTypeKey, TypeValue>) -> Type {
        match &self {
            TypeKind::I8 => Type::I8,
            TypeKind::I16 => Type::I16,
            TypeKind::I32 => Type::I32,
            TypeKind::I64 => Type::I64,
            TypeKind::I128 => Type::I128,
            TypeKind::U8 => Type::U8,
            TypeKind::U16 => Type::U16,
            TypeKind::U32 => Type::U32,
            TypeKind::U64 => Type::U64,
            TypeKind::U128 => Type::U128,
            TypeKind::Bool => Type::Bool,
            TypeKind::F16 => Type::F16,
            TypeKind::F32B => Type::F32B,
            TypeKind::F32 => Type::F32,
            TypeKind::F64 => Type::F64,
            TypeKind::F128 => Type::F128,
            TypeKind::F80 => Type::F80,
            TypeKind::F128PPC => Type::F128PPC,
            TypeKind::Char => Type::U8,
            TypeKind::Array(elem_t, len) => Type::Array(Box::new(elem_t.get_type(type_vals)), *len),
            TypeKind::Void => Type::Void,
            TypeKind::Vague(_) => panic!("Tried to compile a vague type!"),
            TypeKind::CustomType(n) => {
                let type_val = type_vals.get(n).unwrap().clone();
                Type::CustomType(type_val)
            }
            TypeKind::UserPtr(type_kind) => Type::Ptr(Box::new(type_kind.get_type(type_vals))),
            TypeKind::CodegenPtr(type_kind) => Type::Ptr(Box::new(type_kind.get_type(type_vals))),
            TypeKind::Borrow(type_kind, _) => Type::Ptr(Box::new(type_kind.get_type(type_vals))),
        }
    }
}

impl TypeKind {
    pub(super) fn get_debug_type(&self, debug: &Debug, scope: &Scope) -> DebugTypeValue {
        self.get_debug_type_hard(
            &debug.scope,
            debug.info,
            debug.types,
            scope.type_map,
            scope.module_id,
            scope.tokens,
            scope.modules,
        )
    }

    pub(super) fn get_debug_type_hard(
        &self,
        scope: &DebugScopeValue,
        debug_info: &DebugInformation,
        debug_types: &HashMap<TypeKind, DebugTypeValue>,
        type_map: &HashMap<CustomTypeKey, TypeDefinition>,
        local_mod: SourceModuleId,
        tokens: &Vec<FullToken>,
        modules: &HashMap<SourceModuleId, ModuleCodegen>,
    ) -> DebugTypeValue {
        if let Some(ty) = debug_types.get(self) {
            return *ty;
        }

        let name = format!("{}", self);

        let data = match self {
            TypeKind::CodegenPtr(inner) | TypeKind::UserPtr(inner) | TypeKind::Borrow(inner, _) => {
                DebugTypeData::Pointer(DebugPointerType {
                    name,
                    pointee: inner.get_debug_type_hard(
                        scope,
                        debug_info,
                        debug_types,
                        type_map,
                        local_mod,
                        tokens,
                        modules,
                    ),
                    size_bits: self.size_of(type_map),
                })
            }
            TypeKind::Array(elem_ty, len) => {
                let elem_ty = elem_ty.clone().get_debug_type_hard(
                    scope,
                    debug_info,
                    debug_types,
                    type_map,
                    local_mod,
                    tokens,
                    modules,
                );
                DebugTypeData::Array(DebugArrayType {
                    size_bits: self.size_of(type_map),
                    align_bits: self.alignment(),
                    element_type: elem_ty,
                    length: *len,
                })
            }
            TypeKind::CustomType(key) => {
                let typedef = type_map.get(key).unwrap();
                match &typedef.kind {
                    TypeDefinitionKind::Struct(struct_type) => {
                        let mut fields = Vec::new();
                        let mut size_bits = 0;

                        for field in &struct_type.0 {
                            let location = if typedef.source_module != local_mod {
                                None
                            } else {
                                field.2.into_debug(&tokens, &scope)
                            };
                            fields.push(DebugFieldType {
                                name: field.0.clone(),
                                scope: scope.clone(),
                                pos: location.map(|l| l.pos),
                                size_bits: field.1.size_of(type_map),
                                offset: size_bits,
                                flags: DwarfFlags,
                                ty: field.1.get_debug_type_hard(
                                    scope,
                                    debug_info,
                                    debug_types,
                                    type_map,
                                    local_mod,
                                    tokens,
                                    modules,
                                ),
                            });
                            size_bits += field.1.size_of(type_map);
                        }
                        {
                            let location = if typedef.source_module != local_mod {
                                None
                            } else {
                                typedef.meta.into_debug(&tokens, scope)
                            };
                            DebugTypeData::Struct(DebugStructType {
                                name: key.0.clone(),
                                scope: scope.clone(),
                                pos: location.map(|l| l.pos),
                                size_bits,
                                flags: DwarfFlags,
                                fields,
                            })
                        }
                    }
                }
            }
            _ => DebugTypeData::Basic(DebugBasicType {
                name,
                size_bits: self.size_of(type_map),
                encoding: match self {
                    TypeKind::Bool => DwarfEncoding::Boolean,
                    TypeKind::I8 => DwarfEncoding::SignedChar,
                    TypeKind::U8 => DwarfEncoding::UnsignedChar,
                    TypeKind::I16 | TypeKind::I32 | TypeKind::I64 | TypeKind::I128 => DwarfEncoding::Signed,
                    TypeKind::U16 | TypeKind::U32 | TypeKind::U64 | TypeKind::U128 => DwarfEncoding::Unsigned,
                    TypeKind::F16
                    | TypeKind::F32
                    | TypeKind::F32B
                    | TypeKind::F64
                    | TypeKind::F80
                    | TypeKind::F128
                    | TypeKind::F128PPC => DwarfEncoding::Float,
                    TypeKind::Void => DwarfEncoding::Address,
                    TypeKind::Char => DwarfEncoding::UnsignedChar,
                    TypeKind::Array(_, _) => DwarfEncoding::Address,
                    TypeKind::CustomType(_) => DwarfEncoding::Address,
                    _ => panic!("tried fetching debug-type for non-supported type!"),
                },
                flags: DwarfFlags,
            }),
        };
        debug_info.debug_type(data)
    }
}

impl Metadata {
    pub(super) fn into_debug(&self, tokens: &Vec<FullToken>, scope: &DebugScopeValue) -> Option<DebugLocation> {
        if let Some((start, _)) = self.into_positions(tokens) {
            Some(start.debug(scope.clone()))
        } else {
            None
        }
    }
}

impl Position {
    pub(super) fn debug(self, scope: DebugScopeValue) -> DebugLocation {
        DebugLocation {
            pos: DebugPosition {
                line: self.1,
                column: self.0,
            },
            scope,
        }
    }
}
