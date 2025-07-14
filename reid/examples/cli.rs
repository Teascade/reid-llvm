use std::{env, fs, path::PathBuf};

use reid::compile;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if let Some(filename) = args.get(1) {
        let path = PathBuf::from(filename);

        let text = fs::read_to_string(&path)?;
        let output = match compile(&text, PathBuf::from(path)) {
            Ok(t) => t,
            Err(e) => panic!("{}", e),
        };
        println!("{}", output);
    } else {
        println!("Please input compiled file path!")
    }
    Ok(())
}
