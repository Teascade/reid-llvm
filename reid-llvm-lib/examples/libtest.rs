use reid_lib::{
    Context, IntPredicate,
    types::{BasicType, IntegerType, IntegerValue},
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

    let secondary = module.add_function(int_32.function_type(&[]), "secondary");
    let s_entry = secondary.block("entry");
    s_entry.ret(&int_32.from_signed(54)).unwrap();

    let function = module.add_function(int_32.function_type(&[]), "main");

    let entry = function.block("entry");

    let call = entry.call(&secondary, vec![], "call").unwrap();
    let add = entry.add(&int_32.from_signed(100), &call, "add").unwrap();
    let rhs_cmp = int_32.from_signed(200);

    let cond_res = entry
        .integer_compare(&add, &rhs_cmp, &IntPredicate::SLT, "cmp")
        .unwrap();

    let (lhs, rhs) = entry.conditional_br(&cond_res, "lhs", "rhs").unwrap();

    let left = lhs.add(&call, &int_32.from_signed(20), "add").unwrap();
    let right = rhs.add(&call, &int_32.from_signed(30), "add").unwrap();

    let final_block = function.block("final");
    let phi = final_block
        .phi::<IntegerValue>(&int_32, "phi")
        .unwrap()
        .add_incoming(&left, &lhs)
        .add_incoming(&right, &rhs)
        .build();

    lhs.br(&final_block).unwrap();
    rhs.br(&final_block).unwrap();

    let val = final_block
        .add(&phi, &int_32.from_signed(11), "add")
        .unwrap();
    final_block.ret(&val).unwrap();

    match module.print_to_string() {
        Ok(v) => println!("{}", v),
        Err(e) => println!("Err: {:?}", e),
    }
}
