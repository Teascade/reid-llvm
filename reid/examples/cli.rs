use std::{env, fs};

use reid::compile;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if let Some(filename) = args.get(1) {
        let text = fs::read_to_string(filename)?;
        let output = match compile(&text) {
            Ok(t) => t,
            Err(e) => panic!("{}", e),
        };
        println!("{}", output);
    } else {
        println!("Please input compiled file path!")
    }
    Ok(())
}
