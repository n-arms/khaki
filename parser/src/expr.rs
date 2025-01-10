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
