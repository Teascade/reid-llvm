use reid::compile;

pub static EASY: &str = include_str!("./reid/easy.reid");

fn main() {
    let text = match compile(EASY) {
        Ok(t) => t,
        Err(e) => panic!("{}", e),
    };
    println!("{}", text);
}
