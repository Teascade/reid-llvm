mod llvm;

use std::collections::HashMap;

use llvm::{Error, IRBlock, IRContext, IRFunction, IRModule, IRValue};

use crate::{
    ast::{
        Block, BlockLevelStatement, Expression, ExpressionKind, FunctionDefinition, IfExpression,
        LetStatement, ReturnType,
    },
    TopLevelStatement,
};

pub fn form_context() -> IRContext {
    IRContext::new()
}

pub fn from_statements(
    context: &mut IRContext,
    statements: Vec<TopLevelStatement>,
) -> Result<IRModule, Error> {
    let mut module = context.module("testmod".to_owned());
    let mut scope = ScopeData::new();
    for statement in statements {
        statement.codegen(&mut scope, &mut module);
    }

    Ok(module)
}

impl TopLevelStatement {
    fn codegen(&self, scope: &mut ScopeData, module: &mut IRModule) {
        match self {
            Self::FunctionDefinition(func) => func.codegen(scope, module),
            Self::Import(_) => panic!("not implemented"),
        }
    }
}

impl FunctionDefinition {
    fn codegen(&self, scope: &mut ScopeData, module: &mut IRModule) {
        let FunctionDefinition(signature, block, _) = self;
        let ir_function = IRFunction::new(&signature.name, module);

        let ir_block = IRBlock::new(&ir_function, c"entry");
        let mut scope = scope.inner(ir_block);
        if let Some((_, val)) = block.codegen(&mut scope) {
            scope.block.add_return(Some(val));
        } else {
            scope.block.add_return(None);
        }
    }
}

impl Block {
    #[must_use]
    fn codegen(&self, scope: &mut Scope) -> Option<(ReturnType, IRValue)> {
        for statement in &self.0 {
            statement.codegen(scope);
        }

        if let Some((ret_type, return_exp)) = &self.1 {
            let value = return_exp.codegen(scope);
            Some((*ret_type, value))
        } else {
            None
        }
    }
}

impl BlockLevelStatement {
    fn codegen(&self, scope: &mut Scope) {
        use BlockLevelStatement::*;
        match self {
            Expression(exp) | Return(ReturnType::Soft, exp) => {
                exp.codegen(scope);
            }
            Let(LetStatement(name, exp, _)) => {
                let val = exp.codegen(scope);
                scope.data.insert(name, val);
            }
            Return(ReturnType::Hard, _) => panic!("hard returns here should not be possible.."),
            Import(_) => panic!("block level import not supported"),
        }
    }
}

impl Expression {
    fn codegen(&self, scope: &mut Scope) -> IRValue {
        let Expression(kind, _) = self;

        use ExpressionKind::*;
        match kind {
            Literal(lit) => IRValue::from_literal(lit, &scope.block.function.module),
            VariableName(v) => scope.data.fetch(v),
            Binop(op, lhs, rhs) => {
                let lhs = lhs.codegen(scope);
                let rhs = rhs.codegen(scope);
                use crate::ast::BinaryOperator::*;
                match op {
                    Add => scope.block.add(lhs, rhs).unwrap(),
                    Mult => scope.block.mult(lhs, rhs).unwrap(),
                    LessThan => scope.block.less_than(lhs, rhs).unwrap(),
                    _ => panic!("operator not supported: {:?}", op),
                }
            }
            IfExpr(ifx) => {
                let IfExpression(expr, block, _) = ifx.as_ref();
                let condition = expr.codegen(scope);

                let mut thenb = IRBlock::new(scope.block.function, c"then");
                let mut afterb = IRBlock::new(scope.block.function, c"merge");

                scope.block.branch(condition, &mut thenb, &mut afterb);
                scope.block = afterb;

                let mut then = scope.inner(thenb);
                match block.codegen(&mut then) {
                    Some((ReturnType::Hard, v)) => then.block.add_return(Some(v)),
                    _ => then.block.move_into(&mut scope.block),
                }

                IRValue::from_literal(&crate::ast::Literal::I32(1), scope.block.function.module)
            }
            BlockExpr(_) => panic!("block expr not supported"),
            FunctionCall(_) => panic!("function call expr not supported"),
        }
    }
}

#[derive(Clone)]
struct ScopeData {
    vars: HashMap<String, IRValue>,
}

impl ScopeData {
    fn new() -> ScopeData {
        ScopeData {
            vars: HashMap::new(),
        }
    }

    fn with_block<'a, 'b, 'c>(self, block: IRBlock<'a, 'b, 'c>) -> Scope<'a, 'b, 'c> {
        Scope { data: self, block }
    }

    fn inner<'a, 'b, 'c>(&self, block: IRBlock<'a, 'b, 'c>) -> Scope<'a, 'b, 'c> {
        self.clone().with_block(block)
    }

    fn fetch(&self, name: &String) -> IRValue {
        match self.vars.get(name) {
            Some(val) => val.clone(),
            _ => panic!("No such variable in scope: {}", name),
        }
    }

    fn insert(&mut self, name: &String, value: IRValue) {
        match self.vars.insert(name.clone(), value) {
            Some(_) => panic!("{} was already defined in scope", name),
            _ => {}
        }
    }
}

struct Scope<'a, 'b, 'c> {
    data: ScopeData,
    block: IRBlock<'a, 'b, 'c>,
}

impl<'a, 'b, 'c> Scope<'a, 'b, 'c> {
    fn inner(&self, block: IRBlock<'a, 'b, 'c>) -> Scope<'a, 'b, 'c> {
        self.data.clone().with_block(block)
    }
}
