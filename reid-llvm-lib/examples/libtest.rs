use reid_lib::*;

pub fn main() {
    let context = IRContext::new();
    let module = IRModule::new(&context, &"hello".to_owned());

    let mainfunc = IRFunction::new(&module, &"mainfunc".to_owned());

    let secondary_func = IRFunction::new(&module, &"secondary".to_owned());

    let secondary_block = IRBlock::new(&context, &"secondaryblock".to_owned());
    secondary_block.ret(&secondary_func, IRValue::from_const(&context, 54).into());

    let block = IRBlock::new(&context, &"mainblock".to_owned());

    let lhs_1 = IRValue::from_const(&context, 100);
    let lhs_2 = block.call(&secondary_func);
    let lhs_cmp = block.add(lhs_1.into(), lhs_2.into()).unwrap();
    let rhs_cmp = IRValue::from_const(&context, 200);

    let compare = block.less_than(lhs_cmp.into(), rhs_cmp.into()).unwrap();

    let (lhs, rhs) = block.cond_br(&mainfunc, compare);

    lhs.ret(&mainfunc, IRValue::from_const(&context, 123).into());
    rhs.ret(&mainfunc, IRValue::from_const(&context, 456).into());

    match module.print_to_string() {
        Ok(v) => println!("{}", v),
        Err(e) => println!("Err: {:?}", e),
    }
}
