use std::{
    ffi::{CStr, CString, c_char},
    ptr::null_mut,
    string::FromUtf8Error,
};

use llvm_sys::{
    core::{
        LLVMCreateMemoryBufferWithMemoryRange, LLVMDisposeMemoryBuffer, LLVMGetBufferSize,
        LLVMGetBufferStart,
    },
    error::LLVMDisposeErrorMessage,
    prelude::LLVMMemoryBufferRef,
};

use crate::{
    CompileResult, ErrorKind, Type,
    builder::{Builder, InstructionValue},
};

pub fn into_cstring<T: Into<String>>(value: T) -> CString {
    let string = value.into();
    unsafe { CString::from_vec_with_nul_unchecked((string + "\0").into_bytes()) }
}

pub fn from_cstring(pointer: *mut c_char) -> Option<String> {
    if pointer.is_null() {
        None
    } else {
        unsafe { CStr::from_ptr(pointer).to_str().ok().map(|s| s.to_owned()) }
    }
}

fn cstring_to_err(value: *mut c_char) -> Result<(), String> {
    from_cstring(value)
        .filter(|s| !s.is_empty())
        .map_or(Ok(()), |s| Err(s))
}

/// Utility struct for LLVM's Error Messages, which need to be disposed
/// manually.
pub struct ErrorMessageHolder(*mut c_char);

impl ErrorMessageHolder {
    pub fn null() -> Self {
        ErrorMessageHolder(null_mut())
    }

    pub fn borrow_mut(&mut self) -> *mut *mut c_char {
        &mut self.0
    }

    pub fn into_result(&self) -> Result<(), String> {
        cstring_to_err(self.0)
    }
}

impl Drop for ErrorMessageHolder {
    fn drop(&mut self) {
        unsafe {
            if !self.0.is_null() {
                LLVMDisposeErrorMessage(self.0);
            }
        }
    }
}

/// Utility for creating and handling LLVM MemoryBuffers, needed for printing
/// out ASM and .o -files without relying on LLVM's own API.
pub struct MemoryBufferHolder {
    pub buffer: LLVMMemoryBufferRef,
}

impl MemoryBufferHolder {
    pub fn empty(name: &str) -> MemoryBufferHolder {
        let array = [0i8; 0];
        unsafe {
            let buffer = LLVMCreateMemoryBufferWithMemoryRange(
                array.as_ptr(),
                array.len(),
                into_cstring(name).as_ptr(),
                0,
            );
            MemoryBufferHolder { buffer }
        }
    }

    pub fn as_buffer(&self) -> Vec<u8> {
        unsafe {
            let start = LLVMGetBufferStart(self.buffer);
            let size = LLVMGetBufferSize(self.buffer);

            let mut buff = Vec::with_capacity(size);
            for i in 0..size {
                buff.push(*start.add(i) as u8);
            }
            buff
        }
    }

    pub fn as_string(&self) -> Result<String, FromUtf8Error> {
        String::from_utf8(self.as_buffer())
    }
}

impl Drop for MemoryBufferHolder {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeMemoryBuffer(self.buffer);
        }
    }
}

/// Make sure types for given instructions match. Return Ok(type) if they do,
/// and error otherwise.
pub fn match_types(
    lhs: &InstructionValue,
    rhs: &InstructionValue,
    builder: &Builder,
) -> CompileResult<Type> {
    let lhs_type = lhs.get_type(&builder);
    let rhs_type = rhs.get_type(&builder);
    if let (Ok(lhs_t), Ok(rhs_t)) = (lhs_type, rhs_type) {
        if lhs_t == rhs_t {
            Ok(lhs_t)
        } else {
            Err(ErrorKind::Null)
        }
    } else {
        Err(ErrorKind::Null)
    }
}
