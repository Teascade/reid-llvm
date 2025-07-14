use reid::{
    mir::{self, linker::compile_std},
    perform_all_passes,
};
use util::assert_err;

mod util;

#[test]
fn compiles() {
    let _ = compile_std();
}

#[test]
fn passes_all_passes() {
    let mut std = compile_std();

    // Needed to pass linker-pass
    std.is_main = true;

    assert_err(perform_all_passes(&mut mir::Context {
        modules: vec![std],
        base: Default::default(),
    }));
}
