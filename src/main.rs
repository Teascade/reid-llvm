use crate::{lexer::EASIEST, parser::LetStatement, token_stream::TokenStream};

mod lexer;
mod parser;
mod token_stream;

fn main() {
    let tokens = lexer::tokenize(EASIEST).unwrap();
    let mut token_stream = TokenStream::from(&tokens);

    dbg!(token_stream.parse::<LetStatement>().ok());
    dbg!(token_stream.parse::<LetStatement>().ok());
}
