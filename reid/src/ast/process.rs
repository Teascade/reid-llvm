use std::path::PathBuf;

use crate::{
    ast::{self},
    mir::{self, NamedVariableRef, SourceModuleId, StmtKind, StructField, StructType},
};

impl mir::Context {
    pub fn from(modules: Vec<mir::Module>, base: PathBuf) -> mir::Context {
        mir::Context { modules, base }
    }
}

impl ast::Module {
    pub fn process(&self, module_id: SourceModuleId) -> mir::Module {
        let mut imports = Vec::new();
        let mut functions = Vec::new();
        let mut typedefs = Vec::new();

        use ast::TopLevelStatement::*;
        for stmt in &self.top_level_statements {
            match stmt {
                Import(import) => {
                    imports.push(mir::Import(import.0.clone(), import.1.as_meta(module_id)));
                }
                FunctionDefinition(ast::FunctionDefinition(signature, is_pub, block, range)) => {
                    let def = mir::FunctionDefinition {
                        name: signature.name.clone(),
                        is_pub: *is_pub,
                        is_imported: false,
                        return_type: signature
                            .return_type
                            .clone()
                            .map(|r| r.0.into())
                            .unwrap_or(mir::TypeKind::Void),
                        parameters: signature
                            .args
                            .iter()
                            .cloned()
                            .map(|p| (p.0, p.1.into()))
                            .collect(),
                        kind: mir::FunctionDefinitionKind::Local(
                            block.into_mir(module_id),
                            (*range).as_meta(module_id),
                        ),
                    };
                    functions.push(def);
                }
                ExternFunction(signature) => {
                    let def = mir::FunctionDefinition {
                        name: signature.name.clone(),
                        is_pub: false,
                        is_imported: false,
                        return_type: signature
                            .return_type
                            .clone()
                            .map(|r| r.0.into())
                            .unwrap_or(mir::TypeKind::Void),
                        parameters: signature
                            .args
                            .iter()
                            .cloned()
                            .map(|p| (p.0, p.1.into()))
                            .collect(),
                        kind: mir::FunctionDefinitionKind::Extern(false),
                    };
                    functions.push(def);
                }
                TypeDefinition(ast::TypeDefinition { name, kind, range }) => {
                    let def = mir::TypeDefinition {
                        name: name.clone(),
                        kind: match kind {
                            ast::TypeDefinitionKind::Struct(struct_definition_fields) => {
                                mir::TypeDefinitionKind::Struct(StructType(
                                    struct_definition_fields
                                        .iter()
                                        .map(|s| {
                                            StructField(
                                                s.name.clone(),
                                                s.ty.clone().into(),
                                                s.range.as_meta(module_id),
                                            )
                                        })
                                        .collect(),
                                ))
                            }
                        },
                        meta: (*range).as_meta(module_id),
                    };
                    typedefs.push(def);
                }
            }
        }

        mir::Module {
            name: self.name.clone(),
            module_id: module_id,
            imports,
            functions,
            path: self.path.clone(),
            is_main: self.is_main,
            typedefs,
        }
    }
}

impl ast::Block {
    pub fn into_mir(&self, module_id: SourceModuleId) -> mir::Block {
        let mut mir_statements = Vec::new();

        for statement in &self.0 {
            let (kind, range) = match statement {
                ast::BlockLevelStatement::Let(s_let) => (
                    mir::StmtKind::Let(
                        mir::NamedVariableRef(
                            s_let
                                .1
                                .clone()
                                .map(|t| t.0.into())
                                .unwrap_or(mir::TypeKind::Vague(mir::VagueType::Unknown)),
                            s_let.0.clone(),
                            s_let.4.as_meta(module_id),
                        ),
                        s_let.2,
                        s_let.3.process(module_id),
                    ),
                    s_let.4,
                ),
                ast::BlockLevelStatement::Set(var_ref, expression, range) => (
                    StmtKind::Set(var_ref.process(module_id), expression.process(module_id)),
                    *range,
                ),
                ast::BlockLevelStatement::Import { _i } => todo!(),
                ast::BlockLevelStatement::Expression(e) => {
                    (StmtKind::Expression(e.process(module_id)), e.1)
                }
                ast::BlockLevelStatement::Return(_, e) => {
                    (StmtKind::Expression(e.process(module_id)), e.1)
                }
            };

            mir_statements.push(mir::Statement(kind, range.as_meta(module_id)));
        }

        let return_expression = if let Some(r) = &self.1 {
            Some((r.0.into(), Box::new(r.1.process(module_id))))
        } else {
            None
        };

        mir::Block {
            statements: mir_statements,
            return_expression,
            meta: self.2.as_meta(module_id),
        }
    }
}

impl From<ast::ReturnType> for mir::ReturnKind {
    fn from(value: ast::ReturnType) -> Self {
        match value {
            ast::ReturnType::Soft => mir::ReturnKind::Soft,
            ast::ReturnType::Hard => mir::ReturnKind::Hard,
        }
    }
}

impl ast::Expression {
    fn process(&self, module_id: SourceModuleId) -> mir::Expression {
        let kind = match &self.0 {
            ast::ExpressionKind::VariableName(name) => mir::ExprKind::Variable(NamedVariableRef(
                mir::TypeKind::Vague(mir::VagueType::Unknown),
                name.clone(),
                self.1.as_meta(module_id),
            )),
            ast::ExpressionKind::Literal(literal) => mir::ExprKind::Literal(literal.mir()),
            ast::ExpressionKind::Binop(binary_operator, lhs, rhs) => mir::ExprKind::BinOp(
                binary_operator.mir(),
                Box::new(lhs.process(module_id)),
                Box::new(rhs.process(module_id)),
            ),
            ast::ExpressionKind::FunctionCall(fn_call_expr) => {
                mir::ExprKind::FunctionCall(mir::FunctionCall {
                    name: fn_call_expr.0.clone(),
                    return_type: mir::TypeKind::Vague(mir::VagueType::Unknown),
                    parameters: fn_call_expr
                        .1
                        .iter()
                        .map(|e| e.process(module_id))
                        .collect(),
                })
            }
            ast::ExpressionKind::BlockExpr(block) => {
                mir::ExprKind::Block(block.into_mir(module_id))
            }
            ast::ExpressionKind::IfExpr(if_expression) => {
                let cond = if_expression.0.process(module_id);
                let then_block = if_expression.1.into_mir(module_id);
                let else_block = if let Some(el) = &if_expression.2 {
                    Some(el.into_mir(module_id))
                } else {
                    None
                };
                mir::ExprKind::If(mir::IfExpression(Box::new(cond), then_block, else_block))
            }
            ast::ExpressionKind::Array(expressions) => {
                mir::ExprKind::Array(expressions.iter().map(|e| e.process(module_id)).collect())
            }
            ast::ExpressionKind::Indexed(expression, idx_expr) => mir::ExprKind::Indexed(
                Box::new(expression.process(module_id)),
                mir::TypeKind::Vague(mir::VagueType::Unknown),
                Box::new(idx_expr.process(module_id)),
            ),
            ast::ExpressionKind::StructExpression(struct_init) => mir::ExprKind::Struct(
                struct_init.name.clone(),
                struct_init
                    .fields
                    .iter()
                    .map(|(n, e)| (n.clone(), e.process(module_id)))
                    .collect(),
            ),
            ast::ExpressionKind::Accessed(expression, name) => mir::ExprKind::Accessed(
                Box::new(expression.process(module_id)),
                mir::TypeKind::Vague(mir::VagueType::Unknown),
                name.clone(),
            ),
        };

        mir::Expression(kind, self.1.as_meta(module_id))
    }
}

impl ast::BinaryOperator {
    fn mir(&self) -> mir::BinaryOperator {
        match self {
            ast::BinaryOperator::Add => mir::BinaryOperator::Add,
            ast::BinaryOperator::Minus => mir::BinaryOperator::Minus,
            ast::BinaryOperator::Mult => mir::BinaryOperator::Mult,
            ast::BinaryOperator::And => mir::BinaryOperator::And,
            ast::BinaryOperator::LT => mir::BinaryOperator::Cmp(mir::CmpOperator::LT),
            ast::BinaryOperator::LE => mir::BinaryOperator::Cmp(mir::CmpOperator::LE),
            ast::BinaryOperator::GT => mir::BinaryOperator::Cmp(mir::CmpOperator::GT),
            ast::BinaryOperator::GE => mir::BinaryOperator::Cmp(mir::CmpOperator::GE),
            ast::BinaryOperator::EQ => mir::BinaryOperator::Cmp(mir::CmpOperator::EQ),
            ast::BinaryOperator::NE => mir::BinaryOperator::Cmp(mir::CmpOperator::NE),
        }
    }
}

impl ast::Literal {
    fn mir(&self) -> mir::Literal {
        match &self {
            ast::Literal::Number(v) => mir::Literal::Vague(mir::VagueLiteral::Number(*v)),
            ast::Literal::Bool(v) => mir::Literal::Bool(*v),
            ast::Literal::String(val) => mir::Literal::String(val.clone()),
        }
    }
}

impl From<ast::TypeKind> for mir::TypeKind {
    fn from(value: ast::TypeKind) -> Self {
        match &value {
            ast::TypeKind::Bool => mir::TypeKind::Bool,
            ast::TypeKind::I8 => mir::TypeKind::I8,
            ast::TypeKind::I16 => mir::TypeKind::I16,
            ast::TypeKind::I32 => mir::TypeKind::I32,
            ast::TypeKind::I64 => mir::TypeKind::I64,
            ast::TypeKind::I128 => mir::TypeKind::I128,
            ast::TypeKind::U8 => mir::TypeKind::U8,
            ast::TypeKind::U16 => mir::TypeKind::U16,
            ast::TypeKind::U32 => mir::TypeKind::U32,
            ast::TypeKind::U64 => mir::TypeKind::U64,
            ast::TypeKind::U128 => mir::TypeKind::U128,
            ast::TypeKind::Array(type_kind, length) => {
                mir::TypeKind::Array(Box::new(mir::TypeKind::from(*type_kind.clone())), *length)
            }
            ast::TypeKind::String => mir::TypeKind::StringPtr,
            ast::TypeKind::Custom(_) => todo!("Add processing for custom types"),
        }
    }
}

impl From<ast::Type> for mir::TypeKind {
    fn from(value: ast::Type) -> Self {
        value.0.into()
    }
}
