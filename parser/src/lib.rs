macro_rules! parser {
    ($t:ty) => {
        impl Parser<char, $t, Error = Simple<char>> + Clone
    };
}
use chumsky::{
    prelude::Simple,
    primitive::{end, filter, just},
    recursive::{self, Recursive},
    text::{int, keyword, whitespace},
    Parser,
};
use ir::hir::*;

fn pad<T>(inner: parser!(T)) -> parser!(T) {
    whitespace().ignore_then(inner).then_ignore(whitespace())
}

fn id_char() -> parser!(char) {
    filter(|char: &char| char.is_ascii_alphanumeric() || *char == '_')
}

fn identifier() -> parser!(Identifier) {
    pad(filter(char::is_ascii_lowercase)
        .then(id_char().repeated())
        .map(|(prefix, mut name)| {
            name.insert(0, prefix);
            Identifier {
                name: name.into_iter().collect(),
            }
        }))
}

fn upper_identifier() -> parser!(Identifier) {
    pad(filter(char::is_ascii_uppercase)
        .then(id_char().repeated())
        .map(|(prefix, mut name)| {
            name.insert(0, prefix);
            Identifier {
                name: name.into_iter().collect(),
            }
        }))
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

fn expr() -> parser!(Expr) {
    let mut expr = Recursive::declare();
    let integer = int(10).map(|i: String| Expr::Integer(i.parse().unwrap()));
    let variable = identifier()
        .then(
            just('[')
                .ignore_then(comma_list(typ()))
                .then_ignore(just(']'))
                .or_not(),
        )
        .map(|(name, generics)| Expr::Variable {
            name,
            typ: None,
            generics: generics.unwrap_or_default(),
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
        .map(Expr::Tuple);
    let variant = upper_identifier()
        .then_ignore(just(':'))
        .then_ignore(just(':'))
        .then(identifier())
        .then_ignore(just('('))
        .then(expr.clone())
        .then_ignore(just(')'))
        .map(|((typ, tag), argument)| Expr::Enum {
            typ,
            tag,
            argument: Box::new(argument),
        });
    let match_ = keyword("match")
        .ignore_then(expr.clone())
        .then_ignore(just('{'))
        .then(comma_list(
            identifier()
                .then_ignore(just('('))
                .then(identifier())
                .then_ignore(just(')'))
                .then_ignore(whitespace())
                .then_ignore(just('='))
                .then_ignore(just('>'))
                .then(expr.clone())
                .map(|((variant, binding), body)| MatchCase {
                    variant,
                    binding,
                    binding_type: None,
                    body,
                }),
        ))
        .then_ignore(just('}'))
        .map(|(head, cases)| Expr::Match {
            head: Box::new(head),
            cases,
        });
    let parens = just('(').ignore_then(expr.clone()).then_ignore(just(')'));
    let access = pad(just('.').ignore_then(whitespace()).ignore_then(int(10)));
    let call = pad(just('(')
        .ignore_then(comma_list(expr.clone()))
        .then_ignore(just(')')));

    let addon = access.map(Ok).or(call.map(Err));
    let trivial = pad(integer
        .or(match_)
        .or(variant)
        .or(variable)
        .or(function)
        .or(tuple)
        .or(parens));

    expr.define(
        trivial
            .then(addon.repeated())
            .foldl(|expr, addon| match addon {
                Ok(field) => Expr::TupleAccess(Box::new(expr), field.parse().unwrap()),
                Err(arguments) => Expr::FunctionCall {
                    function: Box::new(expr),
                    set: LambdaSet::dummy(),
                    arguments,
                },
            }),
    );
    expr
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

pub fn parse_program(text: &str) -> Result<Program, Vec<Simple<char>>> {
    program().parse(text)
}
pub fn parse_type(text: &str) -> Result<Type, Vec<Simple<char>>> {
    typ().then_ignore(end()).parse(text)
}
