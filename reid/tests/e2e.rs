use std::{path::PathBuf, process::Command};

use reid::{
    compile_module,
    ld::LDRunner,
    mir::{self},
    parse_module, perform_all_passes,
};
use reid_lib::Context;
use util::assert_err;

mod util;

fn test(source: &str, name: &str, expected_exit_code: i32) {
    assert_err(assert_err(std::panic::catch_unwind(|| {
        let mut map = Default::default();
        let (id, tokens) = assert_err(parse_module(source, name, &mut map));

        let module = assert_err(compile_module(id, tokens, &mut map, None, true));
        let mut mir_context = mir::Context::from(vec![module], Default::default());
        assert_err(perform_all_passes(&mut mir_context, &mut map));

        let context = Context::new(format!("Reid ({})", env!("CARGO_PKG_VERSION")));

        let codegen = assert_err(mir_context.codegen(&context));

        let output = codegen.compile().output();
        let in_path = PathBuf::from("/tmp/temp.o");
        let out_path = in_path.with_extension("out");
        std::fs::write(&in_path, &output.obj_buffer).expect("Could not write OBJ-file!");
        LDRunner::from_command("ld")
            .with_library("c")
            .invoke(&in_path, &out_path);

        let executed = Command::new(&out_path).output().unwrap();
        assert_eq!(expected_exit_code, executed.status.code().unwrap());

        Ok::<(), ()>(())
    })))
}

#[test]
fn arithmetic_compiles_well() {
    test(include_str!("../../examples/arithmetic.reid"), "test", 48);
}
// #[test]
// fn array_compiles_well() {
//     test(include_str!("../../examples/array.reid"), "test");
// }
// #[test]
// fn array_structs_compiles_well() {
//     test(include_str!("../../examples/array_structs.reid"), "test");
// }
// #[test]
// fn borrow_compiles_well() {
//     test(include_str!("../../examples/borrow.reid"), "test");
// }
// #[test]
// fn borrow_hard_compiles_well() {
//     test(include_str!("../../examples/borrow_hard.reid"), "test");
// }
// #[test]
// fn cast_compiles_well() {
//     test(include_str!("../../examples/cast.reid"), "test");
// }
// #[test]
// fn char_compiles_well() {
//     test(include_str!("../../examples/char.reid"), "test");
// }
// #[test]
// fn div_mod_compiles_well() {
//     test(include_str!("../../examples/div_mod.reid"), "test");
// }
// #[test]
// fn fibonacci_compiles_well() {
//     test(include_str!("../../examples/fibonacci.reid"), "test");
// }
// #[test]
// fn float_compiles_well() {
//     test(include_str!("../../examples/float.reid"), "test");
// }
// #[test]
// fn hello_world_compiles_well() {
//     test(include_str!("../../examples/hello_world.reid"), "test");
// }
// #[test]
// fn mutable_compiles_well() {
//     test(include_str!("../../examples/mutable.reid"), "test");
// }

// #[test]
// fn ptr_compiles_well() {
//     test(include_str!("../../examples/ptr.reid"), "test");
// }
// #[test]
// fn std_test_compiles_well() {
//     test(include_str!("../../examples/std_test.reid"), "test");
// }
// #[test]
// fn strings_compiles_well() {
//     test(include_str!("../../examples/strings.reid"), "test");
// }
// #[test]
// fn struct_compiles_well() {
//     test(include_str!("../../examples/struct.reid"), "test");
// }
