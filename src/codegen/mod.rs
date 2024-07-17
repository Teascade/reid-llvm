mod llvm;

use llvm::{IRContext, IRModule};

use crate::TopLevelStatement;

pub fn form_context() -> IRContext {
    IRContext::new()
}

pub fn from_statements<'a>(
    context: &'a mut IRContext,
    statements: Vec<TopLevelStatement>,
) -> Result<IRModule<'a>, Error> {
    Ok(context.module("hello".to_owned()))
}

#[derive(thiserror::Error, Debug)]
pub enum Error {}
