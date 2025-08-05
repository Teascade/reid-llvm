use std::{collections::HashMap, path::PathBuf};

use crate::mir::{
    self, FunctionCall, GlobalKind, GlobalValue, IfExpression, Literal, Module, SourceModuleId, TypeKind,
    WhileStatement,
};

use super::pass::{Pass, PassResult, PassState};

pub trait MacroFunction: std::fmt::Debug {
    fn generate<'ctx, 'a>(
        &self,
        module: &MacroModule,
        params: &[mir::Literal],
        prefix: String,
    ) -> Result<(Vec<GlobalValue>, mir::ExprKind), ErrorKind>;
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorKind {
    #[error("Should never be encountered!")]
    Null,
    #[error("No such macro {0} defined")]
    NoSuchMacro(String),
    #[error("Macro arguments may only be literals")]
    InvalidMacroArgs,
    #[error("Got {0} parameters, expected {1}")]
    InvalidAmountOfParams(u32, u32),
    #[error("Expected argument type of {0}, got {1}")]
    InvalidArgumentType(TypeKind, TypeKind),
    #[error("Error executing macro: {0}")]
    MacroExecutionError(String),
}

type MacroModuleMap = HashMap<SourceModuleId, MacroModule>;

/// Struct used to implement a type-checking pass that can be performed on the
/// MIR.
pub struct MacroPass {
    pub(crate) macros: HashMap<String, Box<dyn MacroFunction>>,
    pub module_map: MacroModuleMap,
}

pub struct MacroModule {
    path: Option<PathBuf>,
}

impl From<&Module> for MacroModule {
    fn from(value: &Module) -> Self {
        MacroModule {
            path: value.path.clone(),
        }
    }
}

type MacroPassState<'map, 'st, 'sc> = PassState<'st, 'sc, (), ErrorKind>;

impl Pass for MacroPass {
    type Data = ();
    type TError = ErrorKind;

    fn context(&mut self, _context: &mut mir::Context, mut _state: PassState<Self::Data, Self::TError>) -> PassResult {
        Ok(())
    }

    fn module(&mut self, module: &mut mir::Module, mut state: PassState<Self::Data, Self::TError>) -> PassResult {
        for function in &mut module.functions {
            let globals = match &mut function.kind {
                mir::FunctionDefinitionKind::Local(block, _) => block.gen_macros(self, &mut state, &self.module_map),
                _ => Vec::new(),
            };

            module.globals.extend(globals);
        }
        Ok(())
    }
}

impl mir::Block {
    fn gen_macros(&mut self, data: &MacroPass, state: &mut MacroPassState, map: &MacroModuleMap) -> Vec<GlobalValue> {
        let mut globals = Vec::new();
        for statement in &mut self.statements {
            globals.extend(statement.gen_macros(data, state, map));
        }
        if let Some((_, Some(return_expr))) = &mut self.return_expression {
            globals.extend(return_expr.gen_macros(data, state, map));
        }
        globals
    }
}

impl mir::Statement {
    fn gen_macros(&mut self, data: &MacroPass, state: &mut MacroPassState, map: &MacroModuleMap) -> Vec<GlobalValue> {
        let mut globals = Vec::new();
        match &mut self.0 {
            mir::StmtKind::Let(.., expr) => {
                globals.extend(expr.gen_macros(data, state, map));
            }
            mir::StmtKind::Set(lhs, rhs) => {
                globals.extend(lhs.gen_macros(data, state, map));
                globals.extend(rhs.gen_macros(data, state, map));
            }
            mir::StmtKind::Import(_) => {}
            mir::StmtKind::Expression(expr) => {
                globals.extend(expr.gen_macros(data, state, map));
            }
            mir::StmtKind::While(WhileStatement { condition, block, .. }) => {
                globals.extend(condition.gen_macros(data, state, map));
                globals.extend(block.gen_macros(data, state, map));
            }
        };
        globals
    }
}

impl mir::Expression {
    fn gen_macros(&mut self, data: &MacroPass, state: &mut MacroPassState, map: &MacroModuleMap) -> Vec<GlobalValue> {
        let mut globals = Vec::new();
        match &mut self.0 {
            mir::ExprKind::FunctionCall(function_call) => {
                for param in &mut function_call.parameters {
                    globals.extend(param.gen_macros(data, state, map));
                }
                if function_call.is_macro {
                    if let Some(existing_macro) = data.macros.get(&function_call.name) {
                        let mut literals = Vec::new();
                        for param in &mut function_call.parameters {
                            match &param.0 {
                                super::ExprKind::Literal(literal) => literals.push(literal.clone()),
                                _ => state.note_errors(&vec![ErrorKind::InvalidMacroArgs], param.1),
                            }
                        }
                        let (generated_globals, expr) = state.or_else(
                            existing_macro
                                .generate(
                                    map.get(&state.scope.module_id.unwrap()).unwrap(),
                                    &literals,
                                    format!(
                                        "macro.{}.{}.{}",
                                        function_call.name, self.1.range.start, self.1.range.end
                                    ),
                                )
                                .map(|(globals, kind)| (globals, mir::Expression(kind, self.1))),
                            (Vec::new(), self.clone()),
                            self.1,
                        );
                        globals.extend(generated_globals);
                        *self = expr;
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
                globals.extend(expression.gen_macros(data, state, map));
                globals.extend(expression1.gen_macros(data, state, map));
            }
            mir::ExprKind::Accessed(expression, ..) => {
                globals.extend(expression.gen_macros(data, state, map));
            }
            mir::ExprKind::Array(expressions) => {
                for expression in expressions {
                    globals.extend(expression.gen_macros(data, state, map));
                }
            }
            mir::ExprKind::Struct(_, items) => {
                for item in items {
                    globals.extend(item.1.gen_macros(data, state, map));
                }
            }
            mir::ExprKind::Literal(_) => {}
            mir::ExprKind::BinOp(_, expression, expression1, _) => {
                globals.extend(expression.gen_macros(data, state, map));
                globals.extend(expression1.gen_macros(data, state, map));
            }
            mir::ExprKind::AssociatedFunctionCall(_, FunctionCall { parameters, .. }) => {
                for expression in parameters {
                    globals.extend(expression.gen_macros(data, state, map));
                }
            }
            mir::ExprKind::If(IfExpression(cond, lhs, rhs)) => {
                globals.extend(cond.gen_macros(data, state, map));
                globals.extend(lhs.gen_macros(data, state, map));
                if let Some(rhs) = rhs.as_mut() {
                    globals.extend(rhs.gen_macros(data, state, map));
                }
            }
            mir::ExprKind::Block(block) => {
                globals.extend(block.gen_macros(data, state, map));
            }
            mir::ExprKind::Borrow(expression, _) => {
                globals.extend(expression.gen_macros(data, state, map));
            }
            mir::ExprKind::Deref(expression) => {
                globals.extend(expression.gen_macros(data, state, map));
            }
            mir::ExprKind::CastTo(expression, _) => {
                globals.extend(expression.gen_macros(data, state, map));
            }
            mir::ExprKind::GlobalRef(..) => {}
        }
        globals
    }
}

pub fn form_macros() -> HashMap<String, Box<dyn MacroFunction>> {
    let mut macros: HashMap<String, Box<dyn MacroFunction>> = HashMap::new();

    macros.insert("include_bytes".to_owned(), Box::new(IncludeBytes));

    macros
}

#[derive(Debug)]
pub struct IncludeBytes;
impl MacroFunction for IncludeBytes {
    fn generate<'ctx, 'a>(
        &self,
        module: &MacroModule,
        literals: &[mir::Literal],
        global_name: String,
    ) -> Result<(Vec<GlobalValue>, mir::ExprKind), ErrorKind> {
        if literals.len() != 1 {
            return Err(ErrorKind::InvalidAmountOfParams(literals.len() as u32, 1));
        }
        let literal = literals.get(0).unwrap();
        let Literal::String(path) = literal else {
            return Err(ErrorKind::InvalidArgumentType(
                literal.as_type(),
                TypeKind::UserPtr(Box::new(TypeKind::Char)),
            ));
        };

        let path = module
            .path
            .as_ref()
            .expect("Module has no path!")
            .parent()
            .expect("Module path has no parent!")
            .join(path);

        let contents = match std::fs::read(path) {
            Ok(content) => content,
            Err(e) => return Err(ErrorKind::MacroExecutionError(format!("{}", e))),
        };

        let literals = contents
            .iter()
            .map(|c| GlobalKind::Literal(Literal::U8(*c)))
            .collect::<Vec<_>>();

        let len = literals.len();

        let global = GlobalValue {
            name: global_name.clone(),
            kind: GlobalKind::Array(literals),
        };

        Ok((
            vec![global.clone()],
            mir::ExprKind::GlobalRef(
                global_name,
                TypeKind::Borrow(Box::new(TypeKind::Array(Box::new(TypeKind::U8), len as u64)), false),
            ),
        ))
    }
}
