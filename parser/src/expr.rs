use crate::{
    parser::{
        brace_list_in, identifier, paren_list, paren_list_in, parser, square_list_in, token,
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
    parsed::{ClosureArgument, Expr, MatchCase, Type, VariableCell},
    token::Kind,
};

pub(crate) fn expr<'a>(env: &'a Env) -> parser!('a, Expr) {
    let mut expr = Recursive::declare();
    let integer =
        token(Kind::Integer).map(|token| Expr::Integer(env[token].parse().unwrap(), token.span));
    let variable = identifier(env).map(|name| Expr::Variable(name, Vec::new(), None));
    let function = square_list_in(identifier(env))
        .then(paren_list(closure_argument(env)))
        .then(token(Kind::ThinArrow).ignore_then(typ(env)).or_not())
        .then_ignore(token(Kind::Equals))
        .then(expr.clone())
        .map(
            |((((start, captures), arguments), result), body): (_, Expr)| Expr::Function {
                captures,
                arguments,
                result,
                set: env.lambda_set(),
                name: env.name("Closure", start.merge(body.span())),
                span: start.merge(body.span()),
                body: Box::new(body),
            },
        );
    let tuple = tuple_list_in(expr.clone()).map(|(span, list)| Expr::Tuple(list, span));
    let variant = upper_identifier(env)
        .then_ignore(token(Kind::DoubleColon))
        .then(identifier(env))
        .then_ignore(token(Kind::LeftParen))
        .then(expr.clone())
        .then(token(Kind::RightParen))
        .map(|(((typ, tag), argument), end)| Expr::Enum {
            tag,
            argument: Box::new(argument),
            span: typ.span.merge(end.span),
            typ,
            generics: Vec::new(),
        });
    let match_case = identifier(env)
        .then_ignore(token(Kind::LeftParen))
        .then(pattern(env))
        .then_ignore(token(Kind::RightParen))
        .then_ignore(token(Kind::ThickArrow))
        .then(expr.clone())
        .map(|((variant, binding), body)| MatchCase {
            span: variant.span.merge(body.span()),
            variant,
            binding,
            body,
            typ: None,
        });
    let match_ = token(Kind::Match)
        .then(expr.clone())
        .then(brace_list_in(match_case))
        .map(|((start, head), (end, cases))| Expr::Match {
            head: Box::new(head),
            cases,
            span: start.span.merge(end),
        });
    let parens = token(Kind::LeftParen)
        .ignore_then(expr.clone())
        .then_ignore(token(Kind::RightParen));
    let access = token(Kind::Period).ignore_then(token(Kind::Integer));
    let call = paren_list_in(expr.clone());

    let addon = access.map(Ok).or(call.map(Err));
    let trivial = integer
        .or(match_)
        .or(variant)
        .or(variable)
        .or(function)
        .or(tuple)
        .or(parens);

    expr.define(
        trivial
            .then(addon.repeated())
            .foldl(|expr, addon| match addon {
                Ok(field) => {
                    let span = expr.span().merge(field.span);
                    Expr::TupleAccess(Box::new(expr), env[field].parse().unwrap(), span)
                }
                Err((span, arguments)) => Expr::FunctionCall {
                    set: env.lambda_set(),
                    arguments,
                    span: expr.span().merge(span),
                    function: Box::new(expr),
                },
            }),
    );
    expr
}

fn closure_argument<'a>(env: &'a Env) -> parser!('a, ClosureArgument) {
    pattern(env)
        .then(token(Kind::Colon).ignore_then(typ(env)).or_not())
        .map(|(binding, typ)| ClosureArgument {
            span: if let Some(typ) = typ.as_ref() {
                binding.span().merge(typ.span())
            } else {
                binding.span()
            },
            typ,
            binding,
        })
}

pub(crate) fn typ<'a>(env: &'a Env) -> parser!('a, Type) {
    recursive::recursive(|typ| {
        let int = token(Kind::Int).map(|token| Type::Integer(token.span));
        let var = identifier(env).map(Type::Rigid);
        let cons = upper_identifier(env)
            .then(square_list_in(typ.clone()).or_not())
            .map(|(name, maybe_generics)| {
                let span = name.span;
                if let Some((end, generics)) = maybe_generics {
                    Type::Constructor(name, generics, span.merge(end))
                } else {
                    Type::Constructor(name, Vec::new(), span)
                }
            });
        let func = paren_list_in(typ.clone())
            .then_ignore(token(Kind::ThinArrow))
            .then(typ.clone())
            .map(|((start, args), result): (_, Type)| {
                let span = start.merge(result.span());
                Type::Function(args, Box::new(result), env.lambda_set(), span)
            });
        let tuple = tuple_list_in(typ.clone()).map(|(span, list)| Type::Tuple(list, span));

        func.or(int).or(var).or(tuple).or(cons)
    })
}
