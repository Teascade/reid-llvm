use crate::{CompileResult, Type, builder::Builder};

#[derive(Clone, Debug)]
pub enum LLVMIntrinsic {
    Max(Type),
    Min(Type),
}

impl LLVMIntrinsic {
    pub fn check(&self, builder: &Builder) -> CompileResult<()> {
        self.signature(builder)?;
        Ok(())
    }

    pub fn signature(&self, builder: &Builder) -> CompileResult<(String, Vec<Type>, Type)> {
        match self {
            LLVMIntrinsic::Max(ty) => {
                let name = match ty.category() {
                    crate::TypeCategory::SignedInteger => format!("llvm.smax.{}", ty.llvm_ty_str(builder)),
                    crate::TypeCategory::UnsignedInteger => format!("llvm.umax.{}", ty.llvm_ty_str(builder)),
                    crate::TypeCategory::Real => format!("llvm.max.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone(), ty.clone()], ty.clone()))
            }
            LLVMIntrinsic::Min(ty) => {
                let name = match ty.category() {
                    crate::TypeCategory::SignedInteger => format!("llvm.smin.{}", ty.llvm_ty_str(builder)),
                    crate::TypeCategory::UnsignedInteger => format!("llvm.umin.{}", ty.llvm_ty_str(builder)),
                    crate::TypeCategory::Real => format!("llvm.min.{}", ty.llvm_ty_str(builder)),
                    _ => return Err(crate::ErrorKind::Null),
                };
                Ok((name, vec![ty.clone(), ty.clone()], ty.clone()))
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
