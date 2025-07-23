use std::{path::PathBuf, process::Command};

pub struct LDRunner {
    command: String,
    dynamic_linker: String,
    libraries: Vec<String>,
}

impl LDRunner {
    pub fn from_command(command: String) -> LDRunner {
        LDRunner {
            command,
            dynamic_linker: "ld-linux-x86-64.so.2".to_string(),
            libraries: Default::default(),
        }
    }

    pub fn with_library(mut self, lib: String) -> LDRunner {
        self.libraries.push(lib);
        self
    }

    pub fn invoke(&self, file: PathBuf) {
        let filepath = file.canonicalize().unwrap();

        let dyn_linker_path = find_objectfile(&self.dynamic_linker);
        let crt1_path = find_objectfile("crt1.o");

        println!("LDRunner: Using dynamic linker at: {:?}", dyn_linker_path);

        let mut ld = Command::new(&self.command);
        ld.arg("-dynamic-linker")
            .arg(dyn_linker_path)
            .arg(crt1_path);

        for library in &self.libraries {
            ld.arg(format!("-l{}", library));
        }

        ld.arg(filepath.to_str().unwrap())
            .arg("-o")
            .arg(filepath.with_extension("out"));

        println!("LDRunner: Executing linker to objfile at {:?}", filepath);
        dbg!(&ld);

        ld.spawn().expect("Unable to execute ld!");
    }
}

fn find_objectfile(name: &str) -> String {
    let whereis = Command::new("whereis")
        .arg(&name)
        .output()
        .expect("Unable to execute whereis");
    let whereis_output = String::from_utf8(whereis.stdout).unwrap();

    whereis_output
        .split(" ")
        .skip(1)
        .next()
        .expect(&format!("Unable to find {}: {}", name, whereis_output))
        .split("\n")
        .next()
        .unwrap()
        .to_owned()
}
