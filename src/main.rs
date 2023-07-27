use crate::lexer::EASIEST;

mod lexer;

fn main() {
    let token_stream = lexer::tokenize(EASIEST).unwrap();

    dbg!(&token_stream);
}
