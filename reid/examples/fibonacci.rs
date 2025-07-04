use reid::compile;

pub static FIBONACCI: &str = include_str!("./reid/fibonacci.reid");

fn main() {
    let text = match compile(FIBONACCI) {
        Ok(t) => t,
        Err(e) => panic!("{}", e),
    };
    println!("{}", text);
}
