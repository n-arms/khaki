macro_rules! parser {
    ($t:ty) => {
        impl Parser<char, $t, Error = Simple<char>> + Clone
    };
}

use chumsky::{
    prelude::Simple,
    primitive::{end, just},
    recursive,
    text::{ident, int, keyword, whitespace},
    Parser,
};
use ir::parsed::*;

fn pad<T>(inner: parser!(T)) -> parser!(T) {
    whitespace().ignore_then(inner).then_ignore(whitespace())
}

fn identifier() -> parser!(Identifier) {
    pad(ident()).map(|name| Identifier { name })
}

fn comma_list<T>(element: parser!(T)) -> parser!(Vec<T>) {
    pad(element
        .clone()
        .then_ignore(just(','))
        .repeated()
        .then(element))
    .map(|(mut first, last)| {
        first.push(last);
        first
    })
    .or_not()
    .map(|list| list.unwrap_or_default())
}

fn typ() -> parser!(Type) {
    recursive::recursive(|typ| {
        let int = keyword("Int").map(|_| Type::Integer);
        let var = identifier().map(|name| Type::Variable(name));
        let func = just('(')
            .ignore_then(comma_list(typ.clone()))
            .then_ignore(just(')'))
            .then_ignore(whitespace())
            .then_ignore(just('-'))
            .then_ignore(just('>'))
            .then(typ.clone())
            .map(|(args, result)| Type::Function(args, Box::new(result), LambdaSet::dummy()));
        let tuple = just('<')
            .ignore_then(just('|'))
            .ignore_then(comma_list(typ.clone()))
            .then_ignore(just('|'))
            .then_ignore(just('>'))
            .map(|elems| Type::Tuple(elems));
        pad(func.or(int).or(var).or(tuple))
    })
}

fn argument() -> parser!(Argument) {
    pad(identifier().then_ignore(just(':')).then(typ())).map(|(name, typ)| Argument { name, typ })
}

fn expr() -> parser!(Expr) {
    recursive::recursive(|expr| {
        let integer = int(10).map(|i: String| Expr::Integer(i.parse().unwrap()));
        let variable = identifier().map(|name| Expr::Variable { name, typ: None });
        let call = identifier()
            .then(
                just('[')
                    .ignore_then(comma_list(typ()))
                    .then_ignore(just(']'))
                    .or_not()
                    .map(|generics| generics.unwrap_or_default()),
            )
            .then_ignore(whitespace())
            .then_ignore(just('('))
            .then(comma_list(expr.clone()).then_ignore(just(')')))
            .map(|((function, generics), arguments)| Expr::FunctionCall {
                function,
                generics,
                arguments,
                set: LambdaSet::dummy(),
            });
        let function = just('[')
            .ignore_then(comma_list(argument()))
            .then_ignore(just(']'))
            .then_ignore(whitespace())
            .then_ignore(just('('))
            .then(comma_list(argument()))
            .then_ignore(just(')'))
            .then_ignore(whitespace())
            .then_ignore(just('-'))
            .then_ignore(just('>'))
            .then_ignore(whitespace())
            .then(typ())
            .then_ignore(whitespace())
            .then_ignore(just('='))
            .then(expr.clone())
            .map(|(((captures, arguments), result), body)| Expr::Function {
                captures,
                arguments,
                result,
                body: Box::new(body),
                set: LambdaSet::dummy(),
                name: Identifier::dummy(),
            });
        let tuple = just('<')
            .ignore_then(just('|'))
            .ignore_then(comma_list(expr.clone()))
            .then_ignore(just('|'))
            .then_ignore(just('>'))
            .map(|elems| Expr::Tuple(elems));
        pad(integer.or(call).or(variable).or(function).or(tuple))
    })
}

fn function() -> parser!(Function) {
    pad(keyword("fn")
        .ignore_then(identifier())
        .then(
            just('[')
                .ignore_then(comma_list(identifier()))
                .then_ignore(just(']'))
                .or_not()
                .map(|generics| generics.unwrap_or_default()),
        )
        .then_ignore(whitespace())
        .then(
            just('(')
                .ignore_then(comma_list(argument()))
                .then_ignore(just(')')),
        )
        .then_ignore(whitespace())
        .then_ignore(just('-'))
        .then_ignore(just('>'))
        .then(typ())
        .then_ignore(just('='))
        .then(expr())
        .map(|((((name, generics), arguments), result), body)| Function {
            name,
            arguments,
            generics,
            result,
            body,
        }))
}

fn program() -> parser!(Vec<Function>) {
    function().repeated().then_ignore(end())
}

pub fn parse_program(text: &str) -> Result<Vec<Function>, Vec<Simple<char>>> {
    program().parse(text)
}
pub fn parse_type(text: &str) -> Result<Type, Vec<Simple<char>>> {
    typ().then_ignore(end()).parse(text)
}
