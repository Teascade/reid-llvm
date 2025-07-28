use std::collections::HashMap;

use crate::codegen::intrinsics::MacroFunction;

use super::pass::{Pass, PassResult, PassState};

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorKind {
    #[error("Should never be encountered!")]
    Null,
}

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct MacroPass {
    macros: HashMap<String, Box<dyn MacroFunction>>,
}

type LinkerPassState<'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

impl Pass for MacroPass {
    type Data = ();
    type TError = ErrorKind;

    fn expr(&mut self, expr: &mut super::Expression, state: PassState<Self::Data, Self::TError>) -> PassResult {
        Ok(())
    }
}
