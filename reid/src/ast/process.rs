use std::path::PathBuf;

use crate::{
    ast::{self},
    mir::{
        self, CustomTypeKey, FunctionParam, ModuleMap, NamedVariableRef, ReturnKind, SourceModuleId, StmtKind,
        StructField, StructType, WhileStatement,
    },
};

impl mir::Context {
    pub fn from(modules: Vec<mir::Module>, base: PathBuf) -> mir::Context {
        let mut map = ModuleMap::new();
        for module in modules {
            map.insert(module.module_id, module);
        }
        mir::Context { modules: map, base }
    }
}

impl ast::Module {
    pub fn process(self, module_id: SourceModuleId) -> mir::Module {
        let mut imports = Vec::new();
        let mut associated_functions = Vec::new();
        let mut functions = Vec::new();
        let mut typedefs = Vec::new();
        let mut binops = Vec::new();

        use ast::TopLevelStatement::*;
        for stmt in &self.top_level_statements {
            match stmt {
                Import(import) => {
                    imports.push(mir::Import(import.0.clone(), import.1.as_meta(module_id)));
                }
                FunctionDefinition(function_def) => functions.push(function_def.into_mir(module_id)),
                ExternFunction(signature) => {
                    let def = mir::FunctionDefinition {
                        name: signature.name.clone(),
                        linkage_name: None,
                        is_pub: false,
                        is_imported: false,
                        return_type: signature
                            .return_type
                            .clone()
                            .map(|r| r.0.into_mir(module_id))
                            .unwrap_or(mir::TypeKind::Void),
                        parameters: signature
                            .params
                            .iter()
                            .cloned()
                            .map(|p| mir::FunctionParam {
                                name: p.0,
                                ty: p.1 .0.into_mir(module_id),
                                meta: p.2.as_meta(module_id),
                            })
                            .collect(),
                        kind: mir::FunctionDefinitionKind::Extern(false),
                        source: Some(module_id),
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
                                                s.ty.clone().0.into_mir(module_id),
                                                s.range.as_meta(module_id),
                                            )
                                        })
                                        .collect(),
                                ))
                            }
                        },
                        meta: (*range).as_meta(module_id),
                        source_module: module_id,
                        importer: None,
                    };
                    typedefs.push(def);
                }
                BinopDefinition(ast::BinopDefinition {
                    lhs,
                    op,
                    rhs,
                    return_ty,
                    block,
                    signature_range,
                }) => {
                    binops.push(mir::BinopDefinition {
                        lhs: mir::FunctionParam {
                            name: lhs.0.clone(),
                            ty: lhs.1 .0.into_mir(module_id),
                            meta: lhs.1 .1.as_meta(module_id),
                        },
                        op: op.mir(),
                        rhs: mir::FunctionParam {
                            name: rhs.0.clone(),
                            ty: rhs.1 .0.into_mir(module_id),
                            meta: rhs.1 .1.as_meta(module_id),
                        },
                        return_type: return_ty.0.into_mir(module_id),
                        fn_kind: mir::FunctionDefinitionKind::Local(
                            block.into_mir(module_id),
                            block.2.as_meta(module_id),
                        ),
                        meta: signature_range.as_meta(module_id),
                        exported: false,
                    });
                }
                AssociatedFunction(ty, function_definition) => {
                    for function_def in function_definition {
                        associated_functions.push((ty.0.into_mir(module_id), function_def.into_mir(module_id)));
                    }
                }
            }
        }

        mir::Module {
            name: self.name.clone(),
            module_id: module_id,
            binop_defs: binops,
            imports,
            associated_functions,
            functions,
            globals: Vec::new(),
            path: self.path.clone(),
            is_main: self.is_main,
            tokens: self.tokens,
            typedefs,
        }
    }
}

impl ast::FunctionDefinition {
    pub fn into_mir(&self, module_id: SourceModuleId) -> mir::FunctionDefinition {
        let &ast::FunctionDefinition(signature, is_pub, block, range) = &self;

        let mut params = Vec::new();
        match &signature.self_kind {
            ast::SelfKind::Borrow(ty) => params.push(mir::FunctionParam {
                name: "self".to_owned(),
                ty: mir::TypeKind::Borrow(Box::new(ty.0.into_mir(module_id)), false),
                meta: ty.1.as_meta(module_id),
            }),
            ast::SelfKind::MutBorrow(ty) => params.push(mir::FunctionParam {
                name: "self".to_owned(),
                ty: mir::TypeKind::Borrow(Box::new(ty.0.into_mir(module_id)), true),
                meta: ty.1.as_meta(module_id),
            }),
            ast::SelfKind::Owned(ty) => params.push(mir::FunctionParam {
                name: "self".to_owned(),
                ty: ty.0.into_mir(module_id),
                meta: ty.1.as_meta(module_id),
            }),
            ast::SelfKind::None => {}
        }

        params.extend(signature.params.iter().cloned().map(|p| FunctionParam {
            name: p.0,
            ty: p.1 .0.into_mir(module_id),
            meta: p.2.as_meta(module_id),
        }));
        mir::FunctionDefinition {
            name: signature.name.clone(),
            linkage_name: None,
            is_pub: *is_pub,
            is_imported: false,
            return_type: signature
                .return_type
                .clone()
                .map(|r| r.0.into_mir(module_id))
                .unwrap_or(mir::TypeKind::Void),
            parameters: params,
            kind: mir::FunctionDefinitionKind::Local(block.into_mir(module_id), (range).as_meta(module_id)),
            source: Some(module_id),
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
                                .ty
                                .clone()
                                .map(|t| t.0.into_mir(module_id))
                                .unwrap_or(mir::TypeKind::Vague(mir::VagueType::Unknown)),
                            s_let.name.clone(),
                            s_let.name_range.as_meta(module_id),
                        ),
                        s_let.mutable,
                        s_let.value.process(module_id),
                    ),
                    s_let.name_range + s_let.value.1,
                ),
                ast::BlockLevelStatement::Set(var_ref, expression, range) => (
                    StmtKind::Set(var_ref.process(module_id), expression.process(module_id)),
                    *range,
                ),
                ast::BlockLevelStatement::Import { _i } => todo!(),
                ast::BlockLevelStatement::Expression(e) => (StmtKind::Expression(e.process(module_id)), e.1),
                ast::BlockLevelStatement::Return(_, e) => {
                    if let Some(e) = e {
                        (StmtKind::Expression(e.process(module_id)), e.1)
                    } else {
                        panic!(); // Should never happen?
                    }
                }
                ast::BlockLevelStatement::ForLoop(counter, counter_range, start, end, block) => {
                    let counter_var = NamedVariableRef(
                        mir::TypeKind::Vague(mir::VagueType::Unknown),
                        counter.clone(),
                        counter_range.as_meta(module_id),
                    );
                    let let_statement = mir::Statement(
                        StmtKind::Let(counter_var.clone(), true, start.process(module_id)),
                        counter_range.as_meta(module_id),
                    );

                    let set_new = mir::Statement(
                        StmtKind::Set(
                            mir::Expression(
                                mir::ExprKind::Variable(counter_var.clone()),
                                counter_range.as_meta(module_id),
                            ),
                            mir::Expression(
                                mir::ExprKind::BinOp(
                                    mir::BinaryOperator::Add,
                                    Box::new(mir::Expression(
                                        mir::ExprKind::Variable(counter_var.clone()),
                                        counter_range.as_meta(module_id),
                                    )),
                                    Box::new(mir::Expression(
                                        mir::ExprKind::Literal(mir::Literal::Vague(mir::VagueLiteral::Number(1))),
                                        counter_range.as_meta(module_id),
                                    )),
                                    mir::TypeKind::Vague(mir::VagueType::Unknown),
                                ),
                                counter_range.as_meta(module_id),
                            ),
                        ),
                        counter_range.as_meta(module_id),
                    );
                    let mut block = block.into_mir(module_id);
                    block.statements.push(set_new);
                    let while_statement = mir::Statement(
                        StmtKind::While(WhileStatement {
                            condition: mir::Expression(
                                mir::ExprKind::BinOp(
                                    mir::BinaryOperator::Cmp(mir::CmpOperator::LT),
                                    Box::new(mir::Expression(
                                        mir::ExprKind::Variable(counter_var),
                                        counter_range.as_meta(module_id),
                                    )),
                                    Box::new(end.process(module_id)),
                                    mir::TypeKind::Vague(mir::VagueType::Unknown),
                                ),
                                counter_range.as_meta(module_id),
                            ),
                            block,
                            meta: self.2.as_meta(module_id),
                        }),
                        self.2.as_meta(module_id),
                    );

                    let inner_scope = StmtKind::Expression(mir::Expression(
                        mir::ExprKind::Block(mir::Block {
                            statements: vec![let_statement, while_statement],
                            return_expression: None,
                            meta: counter_range.as_meta(module_id) + end.1.as_meta(module_id),
                        }),
                        counter_range.as_meta(module_id) + end.1.as_meta(module_id),
                    ));
                    (inner_scope, self.2)
                }
                ast::BlockLevelStatement::WhileLoop(expression, block) => (
                    StmtKind::While(WhileStatement {
                        condition: expression.process(module_id),
                        block: block.into_mir(module_id),
                        meta: self.2.as_meta(module_id),
                    }),
                    self.2,
                ),
            };

            mir_statements.push(mir::Statement(kind, range.as_meta(module_id)));
        }

        let return_expression = if let Some(r) = &self.1 {
            if let Some(expr) = &r.1 {
                Some((r.0.into(), Some(Box::new(expr.process(module_id)))))
            } else {
                Some((ReturnKind::Hard, None))
            }
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
                mir::TypeKind::Vague(mir::VagueType::Unknown),
            ),
            ast::ExpressionKind::FunctionCall(fn_call_expr) => mir::ExprKind::FunctionCall(mir::FunctionCall {
                name: fn_call_expr.name.clone(),
                return_type: mir::TypeKind::Vague(mir::VagueType::Unknown),
                parameters: fn_call_expr.params.iter().map(|e| e.process(module_id)).collect(),
                meta: fn_call_expr.range.as_meta(module_id),
                is_macro: fn_call_expr.is_macro,
            }),
            ast::ExpressionKind::BlockExpr(block) => mir::ExprKind::Block(block.into_mir(module_id)),
            ast::ExpressionKind::IfExpr(if_expression) => {
                let cond = if_expression.0.process(module_id);
                let then_block = if_expression.1.process(module_id);
                let else_block = if let Some(el) = &if_expression.2 {
                    Some(el.process(module_id))
                } else {
                    None
                };
                mir::ExprKind::If(mir::IfExpression(
                    Box::new(cond),
                    Box::new(then_block),
                    Box::new(else_block),
                ))
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
                    .map(|(n, e, r)| (n.clone(), e.process(module_id), r.as_meta(module_id)))
                    .collect(),
            ),
            ast::ExpressionKind::Accessed(expression, name, name_range) => mir::ExprKind::Accessed(
                Box::new(expression.process(module_id)),
                mir::TypeKind::Vague(mir::VagueType::Unknown),
                name.clone(),
                name_range.as_meta(module_id),
            ),
            ast::ExpressionKind::Borrow(expr, mutable) => {
                mir::ExprKind::Borrow(Box::new(expr.process(module_id)), *mutable)
            }
            ast::ExpressionKind::Deref(expr) => mir::ExprKind::Deref(Box::new(expr.process(module_id))),
            ast::ExpressionKind::UnaryOperation(unary_operator, expr) => match unary_operator {
                ast::UnaryOperator::Plus => mir::ExprKind::BinOp(
                    mir::BinaryOperator::Add,
                    Box::new(mir::Expression(
                        mir::ExprKind::Literal(mir::Literal::Vague(mir::VagueLiteral::Number(0))),
                        expr.1.as_meta(module_id),
                    )),
                    Box::new(expr.process(module_id)),
                    mir::TypeKind::Vague(mir::VagueType::Unknown),
                ),
                ast::UnaryOperator::Minus => mir::ExprKind::BinOp(
                    mir::BinaryOperator::Minus,
                    Box::new(mir::Expression(
                        mir::ExprKind::Literal(mir::Literal::Vague(mir::VagueLiteral::Number(0))),
                        expr.1.as_meta(module_id),
                    )),
                    Box::new(expr.process(module_id)),
                    mir::TypeKind::Vague(mir::VagueType::Unknown),
                ),
                ast::UnaryOperator::Not => mir::ExprKind::BinOp(
                    mir::BinaryOperator::Cmp(mir::CmpOperator::EQ),
                    Box::new(expr.process(module_id)),
                    Box::new(mir::Expression(
                        mir::ExprKind::Literal(mir::Literal::Bool(false)),
                        expr.1.as_meta(module_id),
                    )),
                    mir::TypeKind::Bool,
                ),
            },
            ast::ExpressionKind::CastTo(expression, ty) => mir::ExprKind::CastTo(
                Box::new(expression.process(module_id)),
                ty.0.clone().into_mir(module_id),
            ),
            ast::ExpressionKind::ArrayShort(expression, len) => mir::ExprKind::Array(
                vec![*expression.clone(); *len as usize]
                    .iter()
                    .map(|e| e.process(module_id))
                    .collect(),
            ),
            ast::ExpressionKind::AssociatedFunctionCall(ty, fn_call_expr) => mir::ExprKind::AssociatedFunctionCall(
                ty.0.into_mir(module_id),
                mir::FunctionCall {
                    name: fn_call_expr.name.clone(),
                    return_type: mir::TypeKind::Vague(mir::VagueType::Unknown),
                    parameters: fn_call_expr.params.iter().map(|e| e.process(module_id)).collect(),
                    meta: fn_call_expr.range.as_meta(module_id),
                    is_macro: fn_call_expr.is_macro,
                },
            ),
            ast::ExpressionKind::AccessCall(expression, fn_call_expr) => {
                let mut params: Vec<_> = fn_call_expr.params.iter().map(|e| e.process(module_id)).collect();
                params.insert(
                    0,
                    mir::Expression(
                        mir::ExprKind::Borrow(Box::new(expression.process(module_id)), true),
                        expression.1.as_meta(module_id),
                    ),
                );
                if fn_call_expr.is_macro {
                    panic!("Macros aren't supported as access-calls!");
                };

                mir::ExprKind::AssociatedFunctionCall(
                    mir::TypeKind::Vague(mir::VagueType::Unknown),
                    mir::FunctionCall {
                        name: fn_call_expr.name.clone(),
                        return_type: mir::TypeKind::Vague(mir::VagueType::Unknown),
                        parameters: params,
                        meta: fn_call_expr.range.as_meta(module_id),
                        is_macro: fn_call_expr.is_macro,
                    },
                )
            }
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
            ast::BinaryOperator::Div => mir::BinaryOperator::Div,
            ast::BinaryOperator::Mod => mir::BinaryOperator::Mod,
            ast::BinaryOperator::And => mir::BinaryOperator::And,
            ast::BinaryOperator::Or => mir::BinaryOperator::Or,
            ast::BinaryOperator::LT => mir::BinaryOperator::Cmp(mir::CmpOperator::LT),
            ast::BinaryOperator::LE => mir::BinaryOperator::Cmp(mir::CmpOperator::LE),
            ast::BinaryOperator::GT => mir::BinaryOperator::Cmp(mir::CmpOperator::GT),
            ast::BinaryOperator::GE => mir::BinaryOperator::Cmp(mir::CmpOperator::GE),
            ast::BinaryOperator::EQ => mir::BinaryOperator::Cmp(mir::CmpOperator::EQ),
            ast::BinaryOperator::NE => mir::BinaryOperator::Cmp(mir::CmpOperator::NE),
            ast::BinaryOperator::BitshiftRight => mir::BinaryOperator::BitshiftRight,
            ast::BinaryOperator::BitshiftLeft => mir::BinaryOperator::BitshiftLeft,
            ast::BinaryOperator::Xor => mir::BinaryOperator::Xor,
            ast::BinaryOperator::BitAnd => mir::BinaryOperator::BitAnd,
            ast::BinaryOperator::BitOr => mir::BinaryOperator::BitOr,
        }
    }
}

impl ast::Literal {
    fn mir(&self) -> mir::Literal {
        match &self {
            ast::Literal::Integer(v) => mir::Literal::Vague(mir::VagueLiteral::Number(*v)),
            ast::Literal::Bool(v) => mir::Literal::Bool(*v),
            ast::Literal::String(val) => mir::Literal::String(val.clone()),
            ast::Literal::Decimal(v) => mir::Literal::Vague(mir::VagueLiteral::Decimal(*v)),
            ast::Literal::Char(inner) => mir::Literal::Char(*inner),
            ast::Literal::Specific(specific_literal) => match specific_literal {
                ast::SpecificLiteral::I8(val) => mir::Literal::I8(*val),
                ast::SpecificLiteral::I16(val) => mir::Literal::I16(*val),
                ast::SpecificLiteral::I32(val) => mir::Literal::I32(*val),
                ast::SpecificLiteral::I64(val) => mir::Literal::I64(*val),
                ast::SpecificLiteral::I128(val) => mir::Literal::I128(*val),
                ast::SpecificLiteral::U8(val) => mir::Literal::U8(*val),
                ast::SpecificLiteral::U16(val) => mir::Literal::U16(*val),
                ast::SpecificLiteral::U32(val) => mir::Literal::U32(*val),
                ast::SpecificLiteral::U64(val) => mir::Literal::U64(*val),
                ast::SpecificLiteral::U128(val) => mir::Literal::U128(*val),
                ast::SpecificLiteral::F16(val) => mir::Literal::F16(*val),
                ast::SpecificLiteral::F32(val) => mir::Literal::F32(*val),
                ast::SpecificLiteral::F32B(val) => mir::Literal::F32B(*val),
                ast::SpecificLiteral::F64(val) => mir::Literal::F64(*val),
                ast::SpecificLiteral::F80(val) => mir::Literal::F80(*val),
                ast::SpecificLiteral::F128(val) => mir::Literal::F128(*val),
                ast::SpecificLiteral::F128PPC(val) => mir::Literal::F128PPC(*val),
            },
        }
    }
}

impl ast::TypeKind {
    fn into_mir(&self, source_mod: SourceModuleId) -> mir::TypeKind {
        match &self {
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
                mir::TypeKind::Array(Box::new(type_kind.clone().into_mir(source_mod)), *length)
            }
            ast::TypeKind::Custom(name) => mir::TypeKind::CustomType(CustomTypeKey(name.clone(), source_mod)),
            ast::TypeKind::Borrow(type_kind, mutable) => {
                mir::TypeKind::Borrow(Box::new(type_kind.clone().into_mir(source_mod)), *mutable)
            }
            ast::TypeKind::Ptr(type_kind) => mir::TypeKind::UserPtr(Box::new(type_kind.clone().into_mir(source_mod))),
            ast::TypeKind::F16 => mir::TypeKind::F16,
            ast::TypeKind::F32B => mir::TypeKind::F32B,
            ast::TypeKind::F32 => mir::TypeKind::F32,
            ast::TypeKind::F64 => mir::TypeKind::F64,
            ast::TypeKind::F80 => mir::TypeKind::F80,
            ast::TypeKind::F128 => mir::TypeKind::F128,
            ast::TypeKind::F128PPC => mir::TypeKind::F128PPC,
            ast::TypeKind::Char => mir::TypeKind::Char,
            ast::TypeKind::Unknown => mir::TypeKind::Vague(mir::VagueType::Unknown),
        }
    }
}
