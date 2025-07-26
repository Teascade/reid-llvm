use std::{
    alloc::System,
    path::PathBuf,
    process::Command,
    thread,
    time::{Duration, SystemTime},
};

use reid::{
    compile_module,
    ld::LDRunner,
    mir::{self},
    parse_module, perform_all_passes,
};
use reid_lib::Context;
use util::assert_err;

mod util;

fn test(source: &str, name: &str, expected_exit_code: Option<i32>) {
    assert_err(assert_err(std::panic::catch_unwind(|| {
        let mut map = Default::default();
        let (id, tokens) = assert_err(parse_module(source, name, &mut map));

        let module = assert_err(compile_module(id, tokens, &mut map, None, true));
        let mut mir_context = mir::Context::from(vec![module], Default::default());
        assert_err(perform_all_passes(&mut mir_context, &mut map));

        let context = Context::new(format!("Reid ({})", env!("CARGO_PKG_VERSION")));

        let codegen = assert_err(mir_context.codegen(&context));

        let output = codegen.compile(None, Vec::new()).output();
        let time = SystemTime::now();
        let in_path = PathBuf::from(format!(
            "/tmp/temp-{}.o",
            time.duration_since(SystemTime::UNIX_EPOCH).unwrap().as_nanos()
        ));

        std::fs::write(&in_path, &output.obj_buffer).expect("Could not write OBJ-file!");

        let out_path = in_path.with_extension("out");
        LDRunner::from_command("ld")
            .with_library("c")
            .invoke(&in_path, &out_path);
        std::fs::remove_file(in_path).unwrap();

        let executed = Command::new(&out_path).output();
        std::fs::remove_file(out_path).unwrap();

        if let Some(expected_exit_code) = expected_exit_code {
            assert_eq!(expected_exit_code, executed.unwrap().status.code().unwrap());
        }

        Ok::<(), ()>(())
    })))
}

#[test]
fn arithmetic_compiles_well() {
    test(include_str!("../../examples/arithmetic.reid"), "test", Some(48));
}
#[test]
fn array_structs_compiles_well() {
    test(include_str!("../../examples/array_structs.reid"), "test", Some(5));
}
#[test]
fn array_compiles_well() {
    test(include_str!("../../examples/array.reid"), "test", Some(3));
}
#[test]
fn borrow_compiles_well() {
    test(include_str!("../../examples/borrow.reid"), "test", Some(17));
}
#[test]
fn borrow_hard_compiles_well() {
    test(include_str!("../../examples/borrow_hard.reid"), "test", Some(17));
}
#[test]
fn cast_compiles_well() {
    test(include_str!("../../examples/cast.reid"), "test", Some(6));
}
#[test]
fn char_compiles_well() {
    test(include_str!("../../examples/char.reid"), "test", Some(98));
}
#[test]
fn div_mod_compiles_well() {
    test(include_str!("../../examples/div_mod.reid"), "test", Some(12));
}
#[test]
fn fibonacci_compiles_well() {
    test(include_str!("../../examples/fibonacci.reid"), "test", Some(1));
}
#[test]
fn float_compiles_well() {
    test(include_str!("../../examples/float.reid"), "test", Some(1));
}
#[test]
fn hello_world_compiles_well() {
    test(include_str!("../../examples/hello_world.reid"), "test", None);
}
#[test]
fn mutable_compiles_well() {
    test(include_str!("../../examples/mutable.reid"), "test", Some(21));
}
#[test]
fn ptr_compiles_well() {
    test(include_str!("../../examples/ptr.reid"), "test", Some(5));
}
#[test]
fn std_test_compiles_well() {
    test(include_str!("../../examples/std_test.reid"), "test", Some(3));
}
#[test]
fn strings_compiles_well() {
    test(include_str!("../../examples/strings.reid"), "test", Some(5));
}
#[test]
fn struct_compiles_well() {
    test(include_str!("../../examples/struct.reid"), "test", Some(17));
}
#[test]
fn loops_compiles_well() {
    test(include_str!("../../examples/loops.reid"), "test", Some(10));
}
#[test]
fn ptr_hard_compiles_well() {
    test(include_str!("../../examples/ptr_hard.reid"), "test", Some(0));
}
#[test]
fn loop_hard_compiles_well() {
    test(include_str!("../../examples/loop_hard.reid"), "test", Some(0));
}
#[test]
fn custom_binop_compiles_well() {
    test(include_str!("../../examples/custom_binop.reid"), "test", Some(21));
}

#[test]
fn array_short_compiles_well() {
    test(include_str!("../../examples/array_short.reid"), "test", Some(5));
}
#[test]
fn imported_type_compiles_well() {
    test(include_str!("../../examples/imported_type.reid"), "test", Some(0));
}
