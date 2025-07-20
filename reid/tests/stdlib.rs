use reid::{
    mir::{self, linker::compile_std},
    perform_all_passes,
};
use util::assert_err;

mod util;

#[test]
fn compiles() {
    let _ = compile_std(&mut Default::default());
}

#[test]
fn passes_all_passes() {
    let mut map = Default::default();
    let Ok((mut std, _)) = compile_std(&mut map) else {
        panic!()
    };

    // Needed to pass linker-pass
    std.is_main = true;

    assert_err(perform_all_passes(
        &mut mir::Context {
            modules: vec![std],
            base: Default::default(),
        },
        &mut map,
    ));
}
