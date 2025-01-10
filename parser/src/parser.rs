macro_rules! parser {
    ($t:ty) => {
        impl Parser<Token, $t, Error = Simple<Token>> + Clone
    };
}

use chumsky::{
    prelude::Simple,
    primitive::{end, filter, just},
    recursive::{self, Recursive},
    text::{int, keyword, whitespace},
    Parser,
};
use ir::{
    parsed::Identifier,
    token::{Kind, Token},
};

pub(crate) fn comma_list<T>(element: parser!(T)) -> parser!(Vec<T>) {
    element
        .clone()
        .then_ignore(just(Kind::Comma.dummy()))
        .repeated()
        .then(element)
        .map(|(mut first, last)| {
            first.push(last);
            first
        })
        .or_not()
        .map(|list| list.unwrap_or_default())
}

pub(crate) fn square_list<T>(element: parser!(T)) -> parser!(Vec<T>) {
    just(Kind::LeftSquare.dummy())
        .ignore_then(comma_list(element))
        .then_ignore(just(Kind::RightSquare.dummy()))
}

pub(crate) fn paren_list<T>(element: parser!(T)) -> parser!(Vec<T>) {
    just(Kind::LeftParen.dummy())
        .ignore_then(comma_list(element))
        .then_ignore(just(Kind::RightParen.dummy()))
}

pub(crate) fn brace_list<T>(element: parser!(T)) -> parser!(Vec<T>) {
    just(Kind::LeftBrace.dummy())
        .ignore_then(comma_list(element))
        .then_ignore(just(Kind::RightBrace.dummy()))
}

pub(crate) fn identifier() -> parser!(Identifier) {
    just(Kind::LowerIdentifier).map(|token| Identifier::from(token.span))
}
