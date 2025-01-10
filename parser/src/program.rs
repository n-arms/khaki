use crate::parser::parser;

use chumsky::{
    prelude::Simple,
    primitive::{end, filter, just},
    recursive::{self, Recursive},
    text::{int, keyword, whitespace},
    Parser,
};

use ir::token::{Kind, Token};

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

fn program() -> parser!(Program) {
    enum_def()
        .repeated()
        .then(function().repeated())
        .then_ignore(end())
        .map(|(enum_defs, functions)| {
            let enums = enum_defs.into_iter().map(|e| (e.name.clone(), e)).collect();
            Program { functions, enums }
        })
}

fn enum_def() -> parser!(Enum) {
    pad(keyword("enum")
        .ignore_then(upper_identifier())
        .then_ignore(just('{'))
        .then(comma_list(
            identifier()
                .then_ignore(just('('))
                .then(typ())
                .then_ignore(just(')')),
        ))
        .then_ignore(just('}')))
    .map(|(name, cases)| Enum { name, cases })
}

fn argument() -> parser!(Argument) {
    pad(identifier().then_ignore(just(':')).then(typ())).map(|(name, typ)| Argument { name, typ })
}

fn typ() -> parser!(Type) {
    recursive::recursive(|typ| {
        let int = keyword("Int").map(|_| Type::Integer);
        let var = identifier().map(Type::Variable);
        let cons = upper_identifier().map(Type::Constructor);
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
            .map(Type::Tuple);
        pad(func.or(int).or(var).or(tuple).or(cons))
    })
}
