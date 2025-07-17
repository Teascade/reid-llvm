use reid::{
    compile_module,
    mir::{self},
    perform_all_passes,
};
use util::assert_err;

mod util;

pub static ARRAY: &str = include_str!("../../reid_src/array.reid");
pub static FIBONACCI: &str = include_str!("../../reid_src/fibonacci.reid");
pub static HELLO_WORLD: &str = include_str!("../../reid_src/hello_world.reid");
pub static MUTABLE: &str = include_str!("../../reid_src/mutable.reid");
pub static STRINGS: &str = include_str!("../../reid_src/strings.reid");

#[test]
fn array_compiles_well() {
    let module = assert_err(compile_module(
        ARRAY,
        "array".to_owned(),
        Default::default(),
        None,
        true,
    ));

    assert_err(perform_all_passes(&mut mir::Context {
        modules: vec![module],
        base: Default::default(),
    }));
}

#[test]
fn fibonacci_compiles_well() {
    let module = assert_err(compile_module(
        FIBONACCI,
        "fibonacci".to_owned(),
        Default::default(),
        None,
        true,
    ));

    assert_err(perform_all_passes(&mut mir::Context {
        modules: vec![module],
        base: Default::default(),
    }));
}

#[test]
fn hello_world_compiles_well() {
    let module = assert_err(compile_module(
        HELLO_WORLD,
        "hello_world".to_owned(),
        Default::default(),
        None,
        true,
    ));

    assert_err(perform_all_passes(&mut mir::Context {
        modules: vec![module],
        base: Default::default(),
    }));
}

#[test]
fn mutable_compiles_well() {
    let module = assert_err(compile_module(
        MUTABLE,
        "mutable".to_owned(),
        Default::default(),
        None,
        true,
    ));

    assert_err(perform_all_passes(&mut mir::Context {
        modules: vec![module],
        base: Default::default(),
    }));
}

#[test]
fn strings_compiles_well() {
    let module = assert_err(compile_module(
        STRINGS,
        "strings".to_owned(),
        Default::default(),
        None,
        true,
    ));

    assert_err(perform_all_passes(&mut mir::Context {
        modules: vec![module],
        base: Default::default(),
    }));
}
