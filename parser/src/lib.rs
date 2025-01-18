use chumsky::{error::Simple, Parser};
use ir::{parsed::Program, token::Token};
use lexer::next_token;
pub use parser::Env;

mod expr;
mod lexer;
mod parser;
mod pattern;
mod program;

#[derive(Debug)]
pub enum Error {
    LexingFailed,
    ParseError(Vec<Simple<Token>>),
}

pub fn parse_program(env: &Env) -> Result<Program, Error> {
    let text = &env.text;
    println!("parsing {text}");
    let tokens = lex_program(text)?;
    println!("{:?}", tokens);

    program::program(env)
        .parse(tokens)
        .map_err(Error::ParseError)
}

pub fn lex_program(text: &str) -> Result<Vec<Token>, Error> {
    let mut chars = text.char_indices().peekable();

    let mut tokens = Vec::new();

    while let Some(token) = next_token(&mut chars) {
        tokens.push(token);
    }

    if chars.next().is_some() {
        Err(Error::LexingFailed)
    } else {
        Ok(tokens)
    }
}
