use std::{env, fs, path::PathBuf};

use reid::compile_simple;
use reid_lib::compile::CompileOutput;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if let Some(filename) = args.get(1) {
        let path = PathBuf::from(filename).canonicalize().unwrap();
        let parent = path.with_extension("");
        let llvm_ir_path = parent.with_extension("ll");
        let object_path = parent.with_extension("o");
        let asm_path = parent.with_extension("asm");

        let before = std::time::SystemTime::now();

        let text = fs::read_to_string(&path)?;
        match compile_simple(&text, PathBuf::from(&path)) {
            Ok(CompileOutput {
                triple,
                assembly,
                obj_buffer,
                llvm_ir,
            }) => {
                println!("{}", llvm_ir);

                let after = std::time::SystemTime::now();
                println!("Compiled with triple: {}\n", &triple);
                fs::write(&llvm_ir_path, &llvm_ir).expect("Could not write LLVM IR -file!");
                println!("Output LLVM IR to {:?}", llvm_ir_path);
                fs::write(&asm_path, &assembly).expect("Could not write Assembly-file!");
                println!("Output Assembly to {:?}", asm_path);
                fs::write(&object_path, &obj_buffer).expect("Could not write Object-file!");
                println!("Output Object-file to {:?}\n", object_path);
                println!(
                    "Compilation took: {:.2}ms\n",
                    (after.duration_since(before).unwrap().as_micros() as f32) / 1000.
                );
            }
            Err(e) => panic!("{}", e),
        };
    } else {
        println!("Please input compiled file path!")
    }
    Ok(())
}
