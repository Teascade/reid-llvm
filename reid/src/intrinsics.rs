use crate::codegen::{ErrorKind, Scope, StackValue};

#[derive(Debug)]
pub enum InstrinsicKind {}

impl InstrinsicKind {
    pub fn codegen<'ctx, 'a>(&self, mut scope: &mut Scope<'ctx, 'a>) -> Result<(), ErrorKind> {
        Ok(())
    }
}
