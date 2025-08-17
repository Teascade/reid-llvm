use std::{env, fs, path::PathBuf};

use argh::FromArgs;
use log::*;
use reid::{compile_simple, ld::LDRunner, CustomIRs};
use reid_lib::compile::CompileOutput;

#[derive(FromArgs, PartialEq, Debug)]
/// Compile or run a Reid (.reid) file
#[argh(help_triggers("-h", "--help", "help"))]
struct Options {
    #[argh(option, short = 'l', default = "log::Level::Info")]
    /// log level
    log_level: log::Level,
    #[argh(subcommand)]
    command: Command,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum Command {
    Build(BuildOpts),
}

#[derive(FromArgs, PartialEq, Debug)]
/// Build an executable file without running it
#[argh(subcommand, name = "build")]
struct BuildOpts {
    #[argh(option, long = "lib", short = 'l')]
    /// asd
    libraries: Vec<String>,
    #[argh(positional)]
    path: PathBuf,
}

fn main() {
    let options: Options = argh::from_env();
    let mut errlog = stderrlog::new();
    errlog.module(module_path!()).verbosity(options.log_level);
    if options.log_level as u8 > 3 {
        errlog.timestamp(stderrlog::Timestamp::Second);
    }
    errlog.init().unwrap();

    match options.command {
        Command::Build(build) => {
            let cpu = std::env::var("CPU").unwrap_or("generic".to_owned());
            let features = std::env::var("REIDFLAGS").unwrap_or("".to_owned());

            let path = match build.path.canonicalize() {
                Ok(path) => path,
                Err(e) => {
                    error!("{e}");
                    return;
                }
            };

            let parent = path.with_extension("");
            let llvm_ir_path = parent.with_extension("ll");
            let object_path = parent.with_extension("o");
            let llir_path = parent.with_extension("llir");
            let mir_path = parent.with_extension("mir");
            let asm_path = parent.with_extension("asm");

            let before = std::time::SystemTime::now();

            let text = match fs::read_to_string(&path) {
                Ok(text) => text,
                Err(e) => {
                    error!("Could not read file: {e}");
                    return;
                }
            };

            match compile_simple(&text, PathBuf::from(&path), Some(cpu), vec![features]) {
                Ok((
                    CompileOutput {
                        triple: _triple,
                        assembly,
                        obj_buffer,
                        llvm_ir: _llvm_ir,
                    },
                    CustomIRs { llir, mir },
                )) => {
                    #[cfg(feature = "log_output")]
                    {
                        println!("{}", _llvm_ir);
                        println!("Compiled with triple: {}\n", &_triple);
                        println!("Output LLVM IR to {:?}", llvm_ir_path);
                        println!("Output Assembly to {:?}", asm_path);
                        println!("Output Object-file to {:?}\n", object_path);
                        println!("Output LLIR-file to {:?}\n", llir_path);
                        println!("Output MIR-file to {:?}\n", mir_path);
                    }

                    fs::write(&llvm_ir_path, &_llvm_ir).expect("Could not write LLVM IR -file!");
                    fs::write(&asm_path, &assembly).expect("Could not write Assembly-file!");
                    fs::write(&object_path, &obj_buffer).expect("Could not write Object-file!");
                    fs::write(&llir_path, &llir).expect("Could not write LLIR-file!");
                    fs::write(&mir_path, &mir).expect("Could not write MIR-file!");
                    let after = std::time::SystemTime::now();
                    println!(
                        "Compilation took: {:.2}ms\n",
                        (after.duration_since(before).unwrap().as_micros() as f32) / 1000.
                    );

                    println!("Linking {:?}", &object_path);

                    let linker = std::env::var("LD").unwrap_or("ld".to_owned());
                    let mut linker = LDRunner::from_command(&linker).with_library("c").with_library("m");
                    for library in build.libraries {
                        linker = linker.with_library(&library);
                    }
                    linker.invoke(&object_path, &object_path.with_extension("out"));
                }
                Err(e) => panic!("{}", e),
            };
        }
    }
}

// fn main() -> Result<(), std::io::Error> {
//     let args: Vec<String> = env::args().collect();
//     let mut iter = args.into_iter().skip(1);
//     if let Some(filename) = iter.next() {
//         let mut libraries = Vec::new();
//         while let Some(libname) = iter.next() {
//             libraries.push(libname);
//         }

//         let path = PathBuf::from(filename).canonicalize().unwrap();
//         let parent = path.with_extension("");
//         let llvm_ir_path = parent.with_extension("ll");
//         let object_path = parent.with_extension("o");
//         let llir_path = parent.with_extension("llir");
//         let mir_path = parent.with_extension("mir");
//         let asm_path = parent.with_extension("asm");

//         let before = std::time::SystemTime::now();

//         let text = fs::read_to_string(&path)?;

//         let cpu = std::env::var("CPU").unwrap_or("generic".to_owned());
//         let features = std::env::var("REIDFLAGS").unwrap_or("".to_owned());

//         match compile_simple(&text, PathBuf::from(&path), Some(cpu), vec![features]) {
//             Ok((
//                 CompileOutput {
//                     triple: _triple,
//                     assembly,
//                     obj_buffer,
//                     llvm_ir: _llvm_ir,
//                 },
//                 CustomIRs { llir, mir },
//             )) => {
//                 #[cfg(feature = "log_output")]
//                 {
//                     println!("{}", _llvm_ir);
//                     println!("Compiled with triple: {}\n", &_triple);
//                     println!("Output LLVM IR to {:?}", llvm_ir_path);
//                     println!("Output Assembly to {:?}", asm_path);
//                     println!("Output Object-file to {:?}\n", object_path);
//                     println!("Output LLIR-file to {:?}\n", llir_path);
//                     println!("Output MIR-file to {:?}\n", mir_path);
//                 }

//                 fs::write(&llvm_ir_path, &_llvm_ir).expect("Could not write LLVM IR -file!");
//                 fs::write(&asm_path, &assembly).expect("Could not write Assembly-file!");
//                 fs::write(&object_path, &obj_buffer).expect("Could not write Object-file!");
//                 fs::write(&llir_path, &llir).expect("Could not write LLIR-file!");
//                 fs::write(&mir_path, &mir).expect("Could not write MIR-file!");
//                 let after = std::time::SystemTime::now();
//                 println!(
//                     "Compilation took: {:.2}ms\n",
//                     (after.duration_since(before).unwrap().as_micros() as f32) / 1000.
//                 );

//                 println!("Linking {:?}", &object_path);

//                 let linker = std::env::var("LD").unwrap_or("ld".to_owned());
//                 let mut linker = LDRunner::from_command(&linker).with_library("c").with_library("m");
//                 for library in libraries {
//                     linker = linker.with_library(&library);
//                 }
//                 linker.invoke(&object_path, &object_path.with_extension("out"));
//             }
//             Err(e) => panic!("{}", e),
//         };
//     } else {
//         println!("Please input compiled file path!")
//     }
//     Ok(())
// }
