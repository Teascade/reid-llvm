use reid_lib::{
    Context, IntPredicate,
    types::{BasicType, IntegerValue, Value},
};

pub fn main() {
    // Notes from inkwell:
    // - Creating new values should probably just be functions in the context
    // - Creating functions should probably be functions from module
    // - Builder could well be it's own struct
    // - Although, I do like the fact where blocks move the builder by itself..

    let context = Context::new();

    let module = context.module("testmodule");

    let int_32 = context.type_i32();

    let fibonacci = module.add_function(int_32.function_type(vec![int_32.into()]), "fibonacci");
    let mut f_main = fibonacci.block("main");

    let param = fibonacci
        .get_param::<IntegerValue>(0, int_32.into())
        .unwrap();
    let mut cmp = f_main
        .integer_compare(&param, &int_32.from_unsigned(3), &IntPredicate::ULT, "cmp")
        .unwrap();

    let mut done = fibonacci.block("done");
    let mut recurse = fibonacci.block("recurse");
    f_main.conditional_br(&cmp, &done, &recurse).unwrap();

    done.ret(&int_32.from_unsigned(1)).unwrap();

    let minus_one = recurse
        .sub(&param, &int_32.from_unsigned(1), "minus_one")
        .unwrap();
    let minus_two = recurse
        .sub(&param, &int_32.from_unsigned(2), "minus_two")
        .unwrap();
    let one: IntegerValue = recurse
        .call(&fibonacci, vec![Value::Integer(minus_one)], "call_one")
        .unwrap();
    let two = recurse
        .call(&fibonacci, vec![Value::Integer(minus_two)], "call_two")
        .unwrap();

    let add = recurse.add(&one, &two, "add").unwrap();

    recurse.ret(&add).unwrap();

    let main_f = module.add_function(int_32.function_type(Vec::new()), "main");

    let mut main_b = main_f.block("main");
    let call: IntegerValue = main_b
        .call(
            &fibonacci,
            vec![Value::Integer(int_32.from_unsigned(8))],
            "fib_call",
        )
        .unwrap();
    main_b.ret(&call).unwrap();

    // let secondary = module.add_function(int_32.function_type(&[]), "secondary");
    // let s_entry = secondary.block("entry");
    // s_entry.ret(&int_32.from_signed(54)).unwrap();

    // let function = module.add_function(int_32.function_type(&[]), "main");

    // let entry = function.block("entry");

    // let call = entry.call(&secondary, vec![], "call").unwrap();
    // let add = entry.add(&int_32.from_signed(100), &call, "add").unwrap();
    // let rhs_cmp = int_32.from_signed(200);

    // let cond_res = entry
    //     .integer_compare(&add, &rhs_cmp, &IntPredicate::SLT, "cmp")
    //     .unwrap();

    // let (lhs, rhs) = entry.conditional_br(&cond_res, "lhs", "rhs").unwrap();

    // let left = lhs.add(&call, &int_32.from_signed(20), "add").unwrap();
    // let right = rhs.add(&call, &int_32.from_signed(30), "add").unwrap();

    // let final_block = function.block("final");
    // let phi = final_block
    //     .phi::<IntegerValue>(&int_32, "phi")
    //     .unwrap()
    //     .add_incoming(&left, &lhs)
    //     .add_incoming(&right, &rhs)
    //     .build();

    // lhs.br(&final_block).unwrap();
    // rhs.br(&final_block).unwrap();

    // let val = final_block
    //     .add(&phi, &int_32.from_signed(11), "add")
    //     .unwrap();
    // final_block.ret(&val).unwrap();

    match module.print_to_string() {
        Ok(v) => println!("{}", v),
        Err(e) => println!("Err: {:?}", e),
    }
}
