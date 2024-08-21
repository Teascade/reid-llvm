use reid::compile;

pub static ARITHMETIC: &str = include_str!("./reid/arithmetic.reid");

fn main() {
    let text = match compile(ARITHMETIC) {
        Ok(t) => t,
        Err(e) => panic!("{}", e),
    };
    println!("{}", text);
}
