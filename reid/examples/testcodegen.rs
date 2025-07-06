use reid::mir::*;
use reid_lib::Context;

fn main() {
    let fibonacci_name = "fibonacci".to_owned();
    let fibonacci_n = "N".to_owned();

    let fibonacci = FunctionDefinition {
        name: fibonacci_name.clone(),
        parameters: vec![(fibonacci_n.clone(), TypeKind::I32)],
        kind: FunctionDefinitionKind::Local(
            Block {
                statements: vec![Statement(
                    StmtKind::Expression(Expression(
                        ExprKind::If(IfExpression(
                            // If N < 3
                            Box::new(Expression(
                                ExprKind::BinOp(
                                    BinaryOperator::Logic(LogicOperator::GreaterThan),
                                    Box::new(Expression(
                                        ExprKind::Variable(VariableReference(
                                            TypeKind::I32,
                                            "N".to_string(),
                                            Default::default(),
                                        )),
                                        Default::default(),
                                    )),
                                    Box::new(Expression(
                                        ExprKind::Literal(Literal::I32(2)),
                                        Default::default(),
                                    )),
                                ),
                                Default::default(),
                            )),
                            // Then
                            Block {
                                statements: vec![],
                                return_expression: Some((
                                    ReturnKind::Hard,
                                    // return fibonacci(n-1) + fibonacci(n-2)
                                    Box::new(Expression(
                                        ExprKind::BinOp(
                                            BinaryOperator::Add,
                                            // fibonacci(n-1)
                                            Box::new(Expression(
                                                ExprKind::FunctionCall(FunctionCall {
                                                    name: fibonacci_name.clone(),
                                                    return_type: TypeKind::I32,
                                                    parameters: vec![Expression(
                                                        ExprKind::BinOp(
                                                            BinaryOperator::Minus,
                                                            Box::new(Expression(
                                                                ExprKind::Variable(
                                                                    VariableReference(
                                                                        TypeKind::I32,
                                                                        fibonacci_n.clone(),
                                                                        Default::default(),
                                                                    ),
                                                                ),
                                                                Default::default(),
                                                            )),
                                                            Box::new(Expression(
                                                                ExprKind::Literal(Literal::I32(1)),
                                                                Default::default(),
                                                            )),
                                                        ),
                                                        Default::default(),
                                                    )],
                                                }),
                                                Default::default(),
                                            )),
                                            // fibonacci(n-2)
                                            Box::new(Expression(
                                                ExprKind::FunctionCall(FunctionCall {
                                                    name: fibonacci_name.clone(),
                                                    return_type: TypeKind::I32,
                                                    parameters: vec![Expression(
                                                        ExprKind::BinOp(
                                                            BinaryOperator::Minus,
                                                            Box::new(Expression(
                                                                ExprKind::Variable(
                                                                    VariableReference(
                                                                        TypeKind::I32,
                                                                        fibonacci_n.clone(),
                                                                        Default::default(),
                                                                    ),
                                                                ),
                                                                Default::default(),
                                                            )),
                                                            Box::new(Expression(
                                                                ExprKind::Literal(Literal::I32(2)),
                                                                Default::default(),
                                                            )),
                                                        ),
                                                        Default::default(),
                                                    )],
                                                }),
                                                Default::default(),
                                            )),
                                        ),
                                        Default::default(),
                                    )),
                                )),
                                meta: Default::default(),
                            },
                            // No else-block
                            None,
                        )),
                        Default::default(),
                    )),
                    Default::default(),
                )],
                // return 1
                return_expression: Some((
                    ReturnKind::Soft,
                    Box::new(Expression(
                        ExprKind::Literal(Literal::I32(1)),
                        Default::default(),
                    )),
                )),
                meta: Default::default(),
            },
            Default::default(),
        ),
    };

    let main = FunctionDefinition {
        name: "main".to_owned(),
        parameters: vec![],
        kind: FunctionDefinitionKind::Local(
            Block {
                statements: vec![],
                return_expression: Some((
                    ReturnKind::Soft,
                    Box::new(Expression(
                        ExprKind::FunctionCall(FunctionCall {
                            name: fibonacci_name.clone(),
                            return_type: TypeKind::I32,
                            parameters: vec![Expression(
                                ExprKind::Literal(Literal::I32(5)),
                                Default::default(),
                            )],
                        }),
                        Default::default(),
                    )),
                )),
                meta: Default::default(),
            },
            Default::default(),
        ),
    };

    println!("test1");

    let module = Module {
        name: "test module".to_owned(),
        imports: vec![],
        functions: vec![fibonacci, main],
    };

    println!("test2");
    let context = Context::new();
    let codegen_module = module.codegen(&context);

    println!("test3");

    codegen_module.context.compile();
    // match codegen_module.module.print_to_string() {
    //     Ok(v) => println!("{}", v),
    //     Err(e) => println!("Err: {:?}", e),
    // }
}
