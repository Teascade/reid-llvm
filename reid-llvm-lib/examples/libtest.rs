use reid_lib::{Comparison, Context, types::BasicType};

pub fn main() {
    // Notes from inkwell:
    // - Creating new values should probably just be functions in the context
    // - Creating functions should probably be functions from module
    // - Builder could well be it's own struct
    // - Although, I do like the fact where blocks move the builder by itself..

    let context = Context::new();

    let module = context.module("testmodule");

    let int_32 = context.integer_type::<32, true>();

    let secondary = module.add_function(int_32.function_type(&[]), "secondary");
    let s_entry = secondary.block("entry");
    s_entry.ret(&int_32.from_const(54)).unwrap();

    let function = module.add_function(int_32.function_type(&[]), "main");

    let entry = function.block("entry");

    let v1 = int_32.from_const(100);
    let v2 = entry.call(&secondary, vec![], "call").unwrap();
    let lhs_cmp = entry.add(&v1, &v2, "add").unwrap();
    let rhs_cmp = int_32.from_const(200);

    let cond_res = entry
        .integer_compare(&lhs_cmp, &rhs_cmp, &Comparison::LessThan, "cmp")
        .unwrap();

    let (lhs, rhs) = entry.conditional_br(&cond_res, "lhs", "rhs").unwrap();

    lhs.ret(&int_32.from_const(123)).unwrap();
    rhs.ret(&int_32.from_const(456)).unwrap();

    match module.print_to_string() {
        Ok(v) => println!("{}", v),
        Err(e) => println!("Err: {:?}", e),
    }
}
