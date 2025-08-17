use std::{fs, path::PathBuf};

use argh::FromArgs;
use log::*;
use reid::{
    compile_simple,
    ld::{execute, LDRunner},
    CustomIRs,
};
use reid_lib::compile::CompileOutput;

#[derive(FromArgs, PartialEq, Debug)]
/// Compile or run a Reid (.reid) file
#[argh(help_triggers("-h", "--help", "help"))]
struct Options {
    #[argh(option, short = 'l', default = "log::Level::Info")]
    /// log level
    log_level: log::Level,
    #[argh(switch, short = 't')]
    /// should logs be timestamped
    timestamps: bool,
    #[argh(subcommand)]
    command: Command,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum Command {
    Build(BuildOpts),
    Run(RunOpts),
}

#[derive(FromArgs, PartialEq, Debug)]
/// Build an executable file without running it
#[argh(subcommand, name = "build")]
struct BuildOpts {
    #[argh(option, long = "lib", short = 'l')]
    /// additional libraries to link against (with ld)
    libraries: Vec<String>,
    #[argh(positional)]
    path: PathBuf,
}

#[derive(FromArgs, PartialEq, Debug)]
/// Build and then execute the executable
#[argh(subcommand, name = "run")]
struct RunOpts {
    #[argh(option, long = "lib", short = 'l')]
    /// additional libraries to link against (with ld)
    libraries: Vec<String>,
    #[argh(positional)]
    path: PathBuf,
}

fn main() {
    let options: Options = argh::from_env();
    let mut errlog = stderrlog::new();
    errlog.module(module_path!()).verbosity(options.log_level);
    if options.timestamps {
        errlog.timestamp(stderrlog::Timestamp::Second);
    }
    errlog.init().unwrap();

    match &options.command {
        Command::Build(BuildOpts { libraries, path }) | Command::Run(RunOpts { libraries, path }) => {
            let cpu = std::env::var("CPU").unwrap_or("generic".to_owned());
            let features = std::env::var("REIDFLAGS").unwrap_or("".to_owned());

            let path = match path.canonicalize() {
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
            let out_path = object_path.with_extension("out");

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
                    log::trace!("{}", _llvm_ir);
                    log::debug!("Compiled with triple: {}\n", &_triple);
                    log::debug!("Output LLVM IR to {:?}", llvm_ir_path);
                    log::debug!("Output Assembly to {:?}", asm_path);
                    log::debug!("Output Object-file to {:?}\n", object_path);
                    log::debug!("Output LLIR-file to {:?}\n", llir_path);
                    log::debug!("Output MIR-file to {:?}\n", mir_path);

                    fs::write(&llvm_ir_path, &_llvm_ir).expect("Could not write LLVM IR -file!");
                    fs::write(&asm_path, &assembly).expect("Could not write Assembly-file!");
                    fs::write(&object_path, &obj_buffer).expect("Could not write Object-file!");
                    fs::write(&llir_path, &llir).expect("Could not write LLIR-file!");
                    fs::write(&mir_path, &mir).expect("Could not write MIR-file!");
                    let after = std::time::SystemTime::now();
                    log::info!(
                        "Compilation took: {:.2}ms\n",
                        (after.duration_since(before).unwrap().as_micros() as f32) / 1000.
                    );

                    log::info!("Linking {:?}", &object_path);

                    let linker = std::env::var("LD").unwrap_or("ld".to_owned());
                    let mut linker = LDRunner::from_command(&linker).with_library("c").with_library("m");
                    for library in libraries {
                        linker = linker.with_library(&library);
                    }
                    linker.invoke(&object_path, &out_path);
                }
                Err(e) => {
                    log::error!("{e}");
                    return;
                }
            };

            match &options.command {
                Command::Build(_) => {}
                Command::Run(_) => {
                    execute(&out_path);
                }
            }
        }
    }
}
