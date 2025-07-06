use reid_lib::{ConstValue, Context, InstructionKind, IntPredicate, TerminatorKind, Type};

fn main() {
    use ConstValue::*;
    use InstructionKind::*;

    let context = Context::new();

    let mut module = context.module("test");

    let main = module.function("main", Type::I32, Vec::new());
    let mut m_entry = main.block("entry");

    let fibonacci = module.function("fibonacci", Type::I32, vec![Type::I32]);

    let arg = m_entry.build(Constant(I32(5))).unwrap();
    let fibonacci_call = m_entry
        .build(FunctionCall(fibonacci.value(), vec![arg]))
        .unwrap();
    m_entry
        .terminate(TerminatorKind::Ret(fibonacci_call))
        .unwrap();

    let mut f_entry = fibonacci.block("entry");

    let num_3 = f_entry.build(Constant(I32(3))).unwrap();
    let param_n = f_entry.build(Param(0)).unwrap();
    let cond = f_entry
        .build(ICmp(IntPredicate::LessThan, param_n, num_3))
        .unwrap();

    let mut then_b = fibonacci.block("then");
    let mut else_b = fibonacci.block("else");

    f_entry
        .terminate(TerminatorKind::CondBr(cond, then_b.value(), else_b.value()))
        .unwrap();

    let ret_const = then_b.build(Constant(I32(1))).unwrap();
    then_b.terminate(TerminatorKind::Ret(ret_const)).unwrap();

    let const_1 = else_b.build(Constant(I32(1))).unwrap();
    let const_2 = else_b.build(Constant(I32(2))).unwrap();
    let param_1 = else_b.build(Sub(param_n, const_1)).unwrap();
    let param_2 = else_b.build(Sub(param_n, const_2)).unwrap();
    let call_1 = else_b
        .build(FunctionCall(fibonacci.value(), vec![param_1]))
        .unwrap();
    let call_2 = else_b
        .build(FunctionCall(fibonacci.value(), vec![param_2]))
        .unwrap();

    let add = else_b.build(Add(call_1, call_2)).unwrap();

    else_b.terminate(TerminatorKind::Ret(add)).unwrap();

    dbg!(&context);

    context.compile();
}
