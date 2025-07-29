use std::{path::PathBuf, process::Command, thread, time::Duration};

pub struct LDRunner {
    command: String,
    dynamic_linker: String,
    libraries: Vec<String>,
}

impl LDRunner {
    pub fn from_command(command: &str) -> LDRunner {
        LDRunner {
            command: command.to_owned(),
            dynamic_linker: "ld-linux-x86-64.so.2".to_string(),
            libraries: Default::default(),
        }
    }

    pub fn with_library(mut self, lib: &str) -> LDRunner {
        self.libraries.push(lib.to_owned());
        self
    }

    pub fn invoke(&self, input: &PathBuf, out_path: &PathBuf) {
        let input_path = input.canonicalize().unwrap();

        let dyn_linker_path = find_objectfile(&self.dynamic_linker);
        let crt1_path = find_objectfile("crt1.o");

        #[cfg(feature = "log_output")]
        println!("LDRunner: Using dynamic linker at: {:?}", dyn_linker_path);

        let mut ld = Command::new(&self.command);
        ld.arg("-dynamic-linker").arg(dyn_linker_path).arg(crt1_path);

        for library in &self.libraries {
            ld.arg(format!("-l{}", library));
        }

        ld.arg(input_path.to_str().unwrap())
            .arg("-o")
            .arg(out_path.to_str().unwrap());

        #[cfg(feature = "log_output")]
        println!(
            "LDRunner: Executing linker to objfile at {:?} => {:?}",
            input_path, out_path
        );
        #[cfg(feature = "log_output")]
        dbg!(&ld);

        ld.spawn().expect("Unable to execute ld!");

        thread::sleep(Duration::from_millis(100));

        #[cfg(feature = "log_output")]
        println!("Setting executable bit to {:?}..", out_path);
        Command::new("chmod").arg("+x").arg(out_path).spawn().unwrap();
        thread::sleep(Duration::from_millis(100));
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
