use crate::{
    expr::{expr, typ},
    parser::{
        brace_list_in, identifier, paren_list, parser, square_list, token, upper_identifier, Env,
    },
    pattern::pattern,
};

use chumsky::{primitive::end, Parser};

use ir::{
    parsed::{Argument, Enum, EnumCase, Function, Program},
    token::Kind,
};

pub(crate) fn program(env: &Env) -> parser!('_, Program) {
    enum_def(env)
        .repeated()
        .then(function(env).repeated())
        .then_ignore(end())
        .map(|(enum_defs, functions)| {
            let enums = enum_defs.into_iter().map(|e| (e.name.clone(), e)).collect();
            Program { functions, enums }
        })
}

fn function(env: &Env) -> parser!('_, Function) {
    token(Kind::Fn)
        .then(identifier(env))
        .then(
            square_list(identifier(env))
                .or_not()
                .map(Option::unwrap_or_default),
        )
        .then(paren_list(argument(env)))
        .then_ignore(token(Kind::ThinArrow))
        .then(typ(env))
        .then_ignore(token(Kind::Equals))
        .then(expr(env))
        .map(
            |(((((start, name), generics), arguments), result), body)| Function {
                span: start.span.merge(body.span()),
                name,
                arguments,
                generics,
                result,
                body,
                set: env.lambda_set(),
            },
        )
}

fn enum_def(env: &Env) -> parser!('_, Enum) {
    token(Kind::Enum)
        .then(upper_identifier(env))
        .then(
            square_list(identifier(env))
                .or_not()
                .map(|list| list.unwrap_or_default()),
        )
        .then(brace_list_in(
            identifier(env)
                .then_ignore(token(Kind::LeftParen))
                .then(typ(env))
                .then(token(Kind::RightParen))
                .map(|((tag, typ), end)| EnumCase {
                    span: tag.span.merge(end.span),
                    tag,
                    typ,
                }),
        ))
        .map(|(((start, name), generics), (end, cases))| Enum {
            name,
            cases,
            span: start.span.merge(end),
            generics,
        })
}

fn argument(env: &Env) -> parser!('_, Argument) {
    pattern(env)
        .then_ignore(token(Kind::Colon))
        .then(typ(env))
        .map(|(binding, typ)| Argument { typ, binding })
}
