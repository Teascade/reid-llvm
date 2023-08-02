use reid::compile;

pub static EASIEST: &str = include_str!("./reid/easy.reid");

fn main() {
    let text = match compile(EASIEST) {
        Ok(t) => t,
        Err(e) => panic!("{}", e),
    };
    println!("{}", text);
}
