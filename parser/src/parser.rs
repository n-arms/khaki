#![allow(dead_code)]

macro_rules! parser {
    ($l:lifetime, $t:ty) => {
        impl chumsky::Parser<ir::token::Token, $t, Error = chumsky::prelude::Simple<ir::token::Token>> + Clone + $l
    };
}

pub(crate) use parser;

use std::{
    cell::{Cell, RefCell},
    ops::{Index, RangeInclusive},
};

use chumsky::{primitive::filter, Parser};
use ir::{
    parsed::{Identifier, LambdaSet, Span},
    token::{Kind, Token},
};

pub(crate) fn comma_list<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, Vec<T>) {
    element
        .clone()
        .then_ignore(token(Kind::Comma))
        .repeated()
        .then(element)
        .map(|(mut first, last)| {
            first.push(last);
            first
        })
        .or_not()
        .map(|list| list.unwrap_or_default())
}

fn list_with<'a, T: 'a>(
    start: Kind,
    end: Kind,
    element: parser!('a, T),
) -> parser!('a, (Span, Vec<T>)) {
    token(start)
        .then(comma_list(element))
        .then(token(end))
        .map(|((start, list), end)| (start.span.merge(end.span), list))
}

pub(crate) fn square_list_in<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, (Span, Vec<T>)) {
    list_with(Kind::LeftSquare, Kind::RightSquare, element)
}

pub(crate) fn square_list<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, Vec<T>) {
    square_list_in(element).map(|(_, list)| list)
}

pub(crate) fn paren_list_in<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, (Span, Vec<T>)) {
    list_with(Kind::LeftParen, Kind::RightParen, element)
}

pub(crate) fn paren_list<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, Vec<T>) {
    paren_list_in(element).map(|(_, list)| list)
}

pub(crate) fn brace_list_in<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, (Span, Vec<T>)) {
    list_with(Kind::LeftBrace, Kind::RightBrace, element)
}

pub(crate) fn brace_list<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, Vec<T>) {
    brace_list_in(element).map(|(_, list)| list)
}

pub(crate) fn tuple_list_in<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, (Span, Vec<T>)) {
    list_with(Kind::LeftTuple, Kind::RightTuple, element)
}

pub(crate) fn tuple_list<'a, T: 'a>(element: parser!('a, T)) -> parser!('a, Vec<T>) {
    tuple_list_in(element).map(|(_, list)| list)
}

pub(crate) fn identifier(env: &Env) -> parser!('_, Identifier) {
    token(Kind::LowerIdentifier)
        .map(|token| Identifier::new(env[token.span.range()].to_string(), token.span))
}

pub(crate) fn upper_identifier(env: &Env) -> parser!('_, Identifier) {
    token(Kind::UpperIdentifier)
        .map(|token| Identifier::new(env[token.span.range()].to_string(), token.span))
}

pub(crate) fn token<'a>(kind: Kind) -> parser!('a, Token) {
    filter(move |token: &Token| token.kind == kind)
}

pub struct Env {
    pub text: String,
    pub tokens: RefCell<Vec<Token>>,
    lambda_sets: Cell<usize>,
    names: Cell<usize>,
}

impl Env {
    pub fn new(text: String) -> Self {
        Self {
            text,
            tokens: RefCell::new(Vec::new()),
            lambda_sets: Cell::default(),
            names: Cell::default(),
        }
    }

    pub(crate) fn lambda_set(&self) -> LambdaSet {
        let token = self.lambda_sets.get();
        self.lambda_sets.set(token + 1);
        LambdaSet { token }
    }

    pub(crate) fn set_tokens(&self, tokens: Vec<Token>) {
        *self.tokens.borrow_mut().as_mut() = tokens;
    }

    pub(crate) fn name(&self, prefix: &str, span: Span) -> Identifier {
        let token = self.names.get();
        self.names.set(token + 1);
        Identifier::new(format!("{prefix}_{token}"), span)
    }

    pub fn lamda_sets(&self) -> usize {
        self.lambda_sets.get()
    }
}

impl Index<RangeInclusive<usize>> for Env {
    type Output = str;

    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        &self.text[index]
    }
}

impl Index<Span> for Env {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self.text[index.range()]
    }
}

impl Index<Token> for Env {
    type Output = str;

    fn index(&self, index: Token) -> &Self::Output {
        let range = index.span.range();

        &self.text[range]
    }
}
