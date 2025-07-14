use std::{env, fs, path::PathBuf};

use reid::compile;
use reid_lib::compile::CompileOutput;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if let Some(filename) = args.get(1) {
        let path = PathBuf::from(filename).canonicalize().unwrap();
        let parent = path.with_extension("");
        let llvm_ir_path = parent.with_extension("ll");
        let object_path = parent.with_extension("o");
        let asm_path = parent.with_extension("asm");

        let text = fs::read_to_string(&path)?;
        match compile(&text, PathBuf::from(&path)) {
            Ok(CompileOutput {
                triple,
                assembly,
                obj_buffer,
                llvm_ir,
            }) => {
                println!("Compiled with triple: {}", &triple);
                fs::write(&llvm_ir_path, &llvm_ir).expect("Could not write LLVM IR -file!");
                println!("Output LLVM IR to {:?}", llvm_ir_path);
                fs::write(&asm_path, &assembly).expect("Could not write Assembly-file!");
                println!("Output Assembly to {:?}", asm_path);
                fs::write(&object_path, &obj_buffer).expect("Could not write Object-file!");
                println!("Output Object-file to {:?}", object_path);

                println!("{}", llvm_ir);
            }
            Err(e) => panic!("{}", e),
        };
    } else {
        println!("Please input compiled file path!")
    }
    Ok(())
}
