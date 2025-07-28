use std::collections::HashMap;

use crate::mir::{self, FunctionCall, IfExpression, WhileStatement};

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
        Ok(())
    }

    fn module(&mut self, module: &mut mir::Module, mut state: PassState<Self::Data, Self::TError>) -> PassResult {
        for function in &mut module.functions {
            match &mut function.kind {
                mir::FunctionDefinitionKind::Local(block, _) => block.gen_macros(self, &mut state)?,
                _ => {}
            };
        }
        Ok(())
    }
}

impl mir::Block {
    fn gen_macros(&mut self, data: &MacroPass, state: &mut MacroPassState) -> PassResult {
        for statement in &mut self.statements {
            statement.gen_macros(data, state)?;
        }
        if let Some((_, Some(return_expr))) = &mut self.return_expression {
            return_expr.gen_macros(data, state)?;
        }
        Ok(())
    }
}

impl mir::Statement {
    fn gen_macros(&mut self, data: &MacroPass, state: &mut MacroPassState) -> PassResult {
        match &mut self.0 {
            mir::StmtKind::Let(.., expr) => {
                expr.gen_macros(data, state)?;
            }
            mir::StmtKind::Set(lhs, rhs) => {
                lhs.gen_macros(data, state)?;
                rhs.gen_macros(data, state)?;
            }
            mir::StmtKind::Import(_) => {}
            mir::StmtKind::Expression(expr) => {
                expr.gen_macros(data, state)?;
            }
            mir::StmtKind::While(WhileStatement { condition, block, .. }) => {
                condition.gen_macros(data, state)?;
                block.gen_macros(data, state)?;
            }
        };
        Ok(())
    }
}

impl mir::Expression {
    fn gen_macros(&mut self, data: &MacroPass, state: &mut MacroPassState) -> PassResult {
        dbg!("asd?");
        match &mut self.0 {
            mir::ExprKind::FunctionCall(function_call) => {
                if function_call.is_macro {
                    if let Some(existing_macro) = data.macros.get(&function_call.name) {
                        let mut literals = Vec::new();
                        for param in &function_call.parameters {
                            match &param.0 {
                                super::ExprKind::Literal(literal) => literals.push(literal.clone()),
                                _ => state.note_errors(&vec![ErrorKind::InvalidMacroArgs], param.1),
                            }
                        }
                        *self = state.or_else(
                            existing_macro
                                .generate(&literals)
                                .map(|kind| mir::Expression(kind, self.1)),
                            self.clone(),
                            self.1,
                        );
                    } else {
                        state.note_errors(
                            &vec![ErrorKind::NoSuchMacro(function_call.name.clone())],
                            function_call.meta,
                        );
                    }
                }
            }
            mir::ExprKind::Variable(_) => {}
            mir::ExprKind::Indexed(expression, _, expression1) => {
                expression.gen_macros(data, state)?;
                expression1.gen_macros(data, state)?;
            }
            mir::ExprKind::Accessed(expression, ..) => {
                expression.gen_macros(data, state)?;
            }
            mir::ExprKind::Array(expressions) => {
                for expression in expressions {
                    expression.gen_macros(data, state)?;
                }
            }
            mir::ExprKind::Struct(_, items) => {
                for item in items {
                    item.1.gen_macros(data, state)?;
                }
            }
            mir::ExprKind::Literal(_) => {}
            mir::ExprKind::BinOp(_, expression, expression1, _) => {
                expression.gen_macros(data, state)?;
                expression1.gen_macros(data, state)?;
            }
            mir::ExprKind::AssociatedFunctionCall(
                _,
                FunctionCall {
                    parameters, is_macro, ..
                },
            ) => {
                for expression in parameters {
                    expression.gen_macros(data, state)?;
                }
            }
            mir::ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                cond.gen_macros(data, state)?;
                lhs.gen_macros(data, state)?;
                if let Some(rhs) = rhs.as_mut() {
                    rhs.gen_macros(data, state)?;
                }
            }
            mir::ExprKind::Block(block) => {
                block.gen_macros(data, state)?;
            }
            mir::ExprKind::Borrow(expression, _) => {
                expression.gen_macros(data, state)?;
            }
            mir::ExprKind::Deref(expression) => {
                expression.gen_macros(data, state)?;
            }
            mir::ExprKind::CastTo(expression, _) => {
                expression.gen_macros(data, state)?;
            }
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
