use crate::{lexer::Token, parser::TopLevelStatement, token_stream::TokenStream};

pub static EASIEST: &str = include_str!("../reid/easiest.reid");
pub static EASY: &str = include_str!("../reid/easy.reid");
pub static MEDIUM: &str = include_str!("../reid/medium.reid");
pub static HARD: &str = include_str!("../reid/hard.reid");

mod lexer;
mod parser;
mod token_stream;

fn main() {
    let tokens = lexer::tokenize(EASY).unwrap();

    dbg!(&tokens);

    let mut token_stream = TokenStream::from(&tokens);

    while let Ok(statement) = token_stream.parse::<TopLevelStatement>() {
        dbg!(&statement);
    }

    dbg!(token_stream.expect(Token::Eof).ok());
}
