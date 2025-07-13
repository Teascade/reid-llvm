use reid::compile;

pub static ARRAY: &str = include_str!("./reid/array.reid");

fn main() {
    let text = match compile(ARRAY) {
        Ok(t) => t,
        Err(e) => panic!("{}", e),
    };
    println!("{}", text);
}
