use crate::{
    expr::{expr, typ},
    parser::{
        brace_list_in, identifier, paren_list, paren_list_in, parser, square_list, token,
        tuple_list_in, upper_identifier, Env,
    },
    pattern::pattern,
};

use chumsky::{
    prelude::Simple,
    primitive::{end, filter, just},
    recursive::{self, Recursive},
    text::{int, keyword, whitespace},
    Parser,
};

use ir::{
    parsed::{Argument, Enum, EnumCase, Function, Program, Type, VariableCell},
    token::{Kind, Token},
};

pub(crate) fn program<'a>(env: &'a Env) -> parser!('a, Program) {
    enum_def(env)
        .repeated()
        .then(function(env).repeated())
        .then_ignore(end())
        .map(|(enum_defs, functions)| {
            let enums = enum_defs.into_iter().map(|e| (e.name.clone(), e)).collect();
            Program { functions, enums }
        })
}

fn function<'a>(env: &'a Env) -> parser!('a, Function) {
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

fn enum_def<'a>(env: &'a Env) -> parser!('a, Enum) {
    token(Kind::Enum)
        .then(upper_identifier(env))
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
        .map(|((start, name), (end, cases))| Enum {
            name,
            cases,
            span: start.span.merge(end),
            // todo: parse generics
            generics: Vec::new(),
        })
}

fn argument<'a>(env: &'a Env) -> parser!('a, Argument) {
    pattern(env)
        .then_ignore(token(Kind::Colon))
        .then(typ(env))
        .map(|(binding, typ)| Argument { typ, binding })
}
