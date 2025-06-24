use reid_lib::*;

pub fn main() {
    let context = IRContext::new();
    let module = IRModule::new(&context, &"hello".to_owned());

    let mainfunc = IRFunction::new(&module, &"mainfunc".to_owned());

    let block = IRBlock::new(&context, &"mainblock".to_owned());

    let lhs_cmp = IRValue::from_const(&context, 100);
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
