use reid_lib::{CmpPredicate, ConstValue, Context, FunctionFlags, Instr, TerminatorKind, Type};

fn main() {
    use ConstValue::*;
    use Instr::*;

    let context = Context::new("libtest");

    let module = context.module("test", true);

    let main = module.function("main", Type::I32, Vec::new(), FunctionFlags::default());
    let mut m_entry = main.block("entry");

    let fibonacci = module.function(
        "fibonacci",
        Type::I32,
        vec![Type::I32],
        FunctionFlags::default(),
    );

    let arg = m_entry.build("const", Constant(I32(5))).unwrap();
    let fibonacci_call = m_entry
        .build("const", FunctionCall(fibonacci.value(), vec![arg]))
        .unwrap();
    m_entry
        .terminate(TerminatorKind::Ret(fibonacci_call))
        .unwrap();

    let mut f_entry = fibonacci.block("entry");

    let num_3 = f_entry.build("const", Constant(I32(3))).unwrap();
    let param_n = f_entry.build("param", Param(0)).unwrap();
    let cond = f_entry
        .build("cmp", ICmp(CmpPredicate::LT, param_n, num_3))
        .unwrap();

    let mut then_b = fibonacci.block("then");
    let mut else_b = fibonacci.block("else");

    f_entry
        .terminate(TerminatorKind::CondBr(cond, then_b.value(), else_b.value()))
        .unwrap();

    let ret_const = then_b.build("const", Constant(I32(1))).unwrap();
    then_b.terminate(TerminatorKind::Ret(ret_const)).unwrap();

    let const_1 = else_b.build("const", Constant(I32(1))).unwrap();
    let const_2 = else_b.build("const", Constant(I32(2))).unwrap();
    let param_1 = else_b.build("sub", Sub(param_n, const_1)).unwrap();
    let param_2 = else_b.build("sub", Sub(param_n, const_2)).unwrap();
    let call_1 = else_b
        .build("fibonacci", FunctionCall(fibonacci.value(), vec![param_1]))
        .unwrap();
    let call_2 = else_b
        .build("fibonacci", FunctionCall(fibonacci.value(), vec![param_2]))
        .unwrap();

    let add = else_b.build("add", Add(call_1, call_2)).unwrap();

    else_b.terminate(TerminatorKind::Ret(add)).unwrap();

    dbg!(&context);

    context.compile();
}
