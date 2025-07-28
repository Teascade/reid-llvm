use std::collections::HashMap;

use crate::mir;

use super::pass::{Pass, PassResult, PassState};

pub trait MacroFunction: std::fmt::Debug {
    fn generate<'ctx, 'a>(&self, params: &[mir::Literal]) -> Result<mir::ExprKind, ErrorKind>;
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorKind {
    #[error("Should never be encountered!")]
    Null,
    #[error("No such macro {0} defined")]
    NoSuchMacro(String),
    #[error("Macro arguments may only be literals")]
    InvalidMacroArgs,
}

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct MacroPass {
    pub(crate) macros: HashMap<String, Box<dyn MacroFunction>>,
}

type MacroPassState<'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

impl Pass for MacroPass {
    type Data = ();
    type TError = ErrorKind;

    fn context(&mut self, _context: &mut mir::Context, mut _state: PassState<Self::Data, Self::TError>) -> PassResult {
        dbg!("hello??");
        Ok(())
    }

    fn expr(&mut self, expr: &mut super::Expression, mut state: MacroPassState) -> PassResult {
        dbg!("hello??");
        match &expr.0 {
            super::ExprKind::FunctionCall(function_call) => {
                if function_call.is_macro {
                    if let Some(existing_macro) = self.macros.get(&function_call.name) {
                        let mut literals = Vec::new();
                        for param in &function_call.parameters {
                            match &param.0 {
                                super::ExprKind::Literal(literal) => literals.push(literal.clone()),
                                _ => state.note_errors(&vec![ErrorKind::InvalidMacroArgs], param.1),
                            }
                        }
                        *expr = state.or_else(
                            existing_macro
                                .generate(&literals)
                                .map(|kind| mir::Expression(kind, expr.1)),
                            expr.clone(),
                            expr.1,
                        );
                    } else {
                        state.note_errors(
                            &vec![ErrorKind::NoSuchMacro(function_call.name.clone())],
                            function_call.meta,
                        );
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }
}

pub fn form_macros() -> HashMap<String, Box<dyn MacroFunction>> {
    let mut macros: HashMap<String, Box<dyn MacroFunction>> = HashMap::new();

    macros.insert("test_macro".to_owned(), Box::new(TestMacro));

    macros
}

#[derive(Debug)]
pub struct TestMacro;
impl MacroFunction for TestMacro {
    fn generate<'ctx, 'a>(&self, _: &[mir::Literal]) -> Result<mir::ExprKind, ErrorKind> {
        Ok(mir::ExprKind::Literal(mir::Literal::Vague(mir::VagueLiteral::Number(
            5,
        ))))
    }
}
