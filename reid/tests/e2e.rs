use reid::{
    compile_module,
    mir::{self},
    parse_module, perform_all_passes,
};
use util::assert_err;

mod util;

fn test(source: &str, name: &str) {
    let mut map = Default::default();
    let (id, tokens) = assert_err(parse_module(source, name, &mut map));
    let module = assert_err(compile_module(id, &tokens, &mut map, None, true));

    assert_err(perform_all_passes(
        &mut mir::Context {
            modules: vec![module],
            base: Default::default(),
        },
        &mut map,
    ));
}

pub static ARRAY: &str = include_str!("../../reid_src/array.reid");
pub static FIBONACCI: &str = include_str!("../../reid_src/fibonacci.reid");
pub static HELLO_WORLD: &str = include_str!("../../reid_src/hello_world.reid");
pub static MUTABLE: &str = include_str!("../../reid_src/mutable.reid");
pub static STRINGS: &str = include_str!("../../reid_src/strings.reid");
pub static STRUCTS: &str = include_str!("../../reid_src/struct.reid");
pub static ARRAY_STRUCTS: &str = include_str!("../../reid_src/array_structs.reid");
pub static BORROW: &str = include_str!("../../reid_src/borrow.reid");
pub static ARITHMETIC: &str = include_str!("../../reid_src/arithmetic.reid");

#[test]
fn array_compiles_well() {
    test(ARRAY, "array");
}

#[test]
fn fibonacci_compiles_well() {
    test(FIBONACCI, "fibonacci");
}

#[test]
fn hello_world_compiles_well() {
    test(HELLO_WORLD, "hello_world");
}

#[test]
fn mutable_compiles_well() {
    test(MUTABLE, "mutable");
}

#[test]
fn strings_compiles_well() {
    test(STRINGS, "strings");
}

#[test]
fn arrays_compiles_well() {
    test(ARRAY, "array");
}

#[test]
fn struct_compiles_well() {
    test(STRUCTS, "struct");
}

#[test]
fn array_structs_compiles_well() {
    test(ARRAY_STRUCTS, "array_structs");
}

#[test]
fn borrow_structs_compiles_well() {
    test(BORROW, "borrow");
}

#[test]
fn arithmetic_structs_compiles_well() {
    test(ARITHMETIC, "arithmetic");
}
