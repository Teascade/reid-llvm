use crate::{CompileResult, Type, TypeCategory, builder::Builder};

#[derive(Clone, Debug)]
pub enum LLVMIntrinsic {
    Abs(Type),
    Max(Type),
    Min(Type),
    Memcpy(Type),
    Sqrt(Type),
    PowI(Type, Type),
    Pow(Type),
    Sin(Type),
    Cos(Type),
    Tan(Type),
    ASin(Type),
    ACos(Type),
    ATan(Type),
    ATan2(Type),
    SinH(Type),
    CosH(Type),
    TanH(Type),
    Log(Type),
    Log2(Type),
    Log10(Type),
    Copysign(Type),
    Floor(Type),
    Ceil(Type),
    Trunc(Type),
    RoundEven(Type),
    Round(Type),
}

impl LLVMIntrinsic {
    pub(crate) fn signature(&self, builder: &Builder) -> CompileResult<(String, Vec<Type>, Type)> {
        match self {
            LLVMIntrinsic::Max(ty) => {
                let name = match ty.category() {
                    TypeCategory::SignedInteger => format!("llvm.smax.{}", ty.llvm_ty_str(builder)),
                    TypeCategory::UnsignedInteger => format!("llvm.umax.{}", ty.llvm_ty_str(builder)),
                    TypeCategory::Real => format!("llvm.max.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone(), ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Min(ty) => {
                let name = match ty.category() {
                    TypeCategory::SignedInteger => format!("llvm.smin.{}", ty.llvm_ty_str(builder)),
                    TypeCategory::UnsignedInteger => format!("llvm.umin.{}", ty.llvm_ty_str(builder)),
                    TypeCategory::Real => format!("llvm.min.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone(), ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Abs(ty) => {
                let name = match ty.category() {
                    TypeCategory::SignedInteger => format!("llvm.abs.{}", ty.llvm_ty_str(builder)),
                    TypeCategory::UnsignedInteger => format!("llvm.abs.{}", ty.llvm_ty_str(builder)),
                    TypeCategory::Real => format!("llvm.fabs.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone(), Type::Bool], ty.clone()))
            }
            LLVMIntrinsic::Memcpy(ty) => {
                let name = match ty.category() {
                    TypeCategory::Ptr => String::from("llvm.memcpy"),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone(), ty.clone()], Type::Void))
            }
            LLVMIntrinsic::Sqrt(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.sqrt.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::PowI(ty1, ty2) => {
                let name = match (ty1.category(), ty2.category()) {
                    (TypeCategory::Real, TypeCategory::SignedInteger) => {
                        format!("llvm.powi.{}.{}", ty1.llvm_ty_str(builder), ty2.llvm_ty_str(builder))
                    }
                    (TypeCategory::Real, TypeCategory::UnsignedInteger) => {
                        format!("llvm.powi.{}.{}", ty1.llvm_ty_str(builder), ty2.llvm_ty_str(builder))
                    }
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty1.clone(), ty2.clone()], ty1.clone()))
            }
            LLVMIntrinsic::Pow(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.pow.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone(), ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Sin(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.sin.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Cos(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.cos.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Tan(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.tan.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::ASin(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.asin.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::ACos(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.acos.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::ATan(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.atan.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::ATan2(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.atan2.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone(), ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::SinH(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.sinh.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::CosH(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.cosh.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::TanH(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.tanh.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Log(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.log.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Log2(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.log2.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Log10(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.log10.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Copysign(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.copysign.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Floor(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.floor.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Ceil(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.ceil.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Trunc(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.trunc.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::RoundEven(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.roundeven.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Round(ty) => {
                let name = match ty.category() {
                    TypeCategory::Real => format!("llvm.round.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone()], ty.clone()))
            }
        }
    }
}

impl Type {
    fn llvm_ty_str(&self, builder: &Builder) -> String {
        match self {
            Type::I8 => String::from("i8"),
            Type::I16 => String::from("u16"),
            Type::I32 => String::from("i32"),
            Type::I64 => String::from("i64"),
            Type::I128 => String::from("i128"),
            Type::U8 => String::from("i8"),
            Type::U16 => String::from("i16"),
            Type::U32 => String::from("i32"),
            Type::U64 => String::from("i64"),
            Type::U128 => String::from("i128"),
            Type::F16 => String::from("half"),
            Type::F32B => String::from("bfloat"),
            Type::F32 => String::from("float"),
            Type::F64 => String::from("double"),
            Type::F80 => String::from("x86_fp80"),
            Type::F128 => String::from("fp128"),
            Type::F128PPC => String::from("ppc_fp128"),
            Type::Bool => String::from("i1"),
            Type::Void => String::from("void"),
            Type::CustomType(type_value) => {
                let ty = unsafe { builder.type_data(type_value) };
                ty.name.clone()
            }
            Type::Array(ty, len) => format!("[{} x {}]", len, ty.llvm_ty_str(builder)),
            Type::Ptr(_) => String::from("ptr"),
        }
    }
}
