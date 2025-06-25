use std::{
    ffi::{CString, c_char},
    ptr::null_mut,
};

use llvm_sys::error::LLVMDisposeErrorMessage;

pub fn into_cstring<T: Into<String>>(value: T) -> CString {
    let string = value.into();
    unsafe { CString::from_vec_with_nul_unchecked((string + "\0").into_bytes()) }
}

pub fn from_cstring(value: *mut c_char) -> Option<String> {
    if value.is_null() {
        None
    } else {
        unsafe { CString::from_raw(value).into_string().ok() }
    }
}

fn cstring_to_err(value: *mut c_char) -> Result<(), String> {
    from_cstring(value)
        .filter(|s| !s.is_empty())
        .map_or(Ok(()), |s| Err(s))
}

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
