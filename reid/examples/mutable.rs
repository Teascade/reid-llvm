use reid::compile;

pub static MUTABLE: &str = include_str!("./reid/mutable.reid");

fn main() {
    let text = match compile(MUTABLE) {
        Ok(t) => t,
        Err(e) => panic!("{}", e),
    };
    println!("{}", text);
}
