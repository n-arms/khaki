use generator::{Env, Generator};
use ir::parsed::{Expr, Function, Type};
use std::io::Write;

mod generator;

pub fn program(functions: &[Function]) -> String {
    let mut env = Env::default();

    preamble(&mut env);

    for func in functions {
        forward_function(func, &mut env);
    }

    for func in functions {
        function(func, &mut env);
    }

    env.generate()
}

fn preamble(env: &mut Env) {}

fn forward_function(func: &Function, env: &mut Env) {
    let result = typ_to_string(&func.result, env);
    write!(&mut env.main, "{} {:?}(", result, &func.name);
    let args: Vec<_> = func
        .arguments
        .iter()
        .map(|arg| typ_to_string(&arg.typ, env))
        .collect();
    env.main.comma_list(args, |gen, arg| {
        write!(gen, "{}", arg);
    });
    write!(&mut env.main, ");");
    env.main.newline();
}

fn function(func: &Function, env: &mut Env) {
    let result = typ_to_string(&func.result, env);
    write!(&mut env.main, "{} {:?}(", result, &func.name);
    let args: Vec<_> = func
        .arguments
        .iter()
        .map(|arg| typ_to_string(&arg.typ, env))
        .collect();
    env.main
        .comma_list(args.into_iter().zip(&func.arguments), |gen, (typ, arg)| {
            write!(gen, "{} {}", typ, arg.name.name);
        });
    write!(&mut env.main, ") {{");
    env.main.newline();
    env.main.inc();

    let result = expr(&func.body, env);

    write!(&mut env.main, "return {result};");
    env.main.newline();

    env.main.dec();
    write!(&mut env.main, "}}");
    env.main.newline();
}

fn expr(to_gen: &Expr, env: &mut Env) -> String {
    match to_gen {
        Expr::Integer(int) => int.to_string(),
        Expr::Variable { name, .. } => name.name.clone(),
        Expr::FunctionCall {
            function,
            arguments,
            ..
        } => {
            let mut output = format!("{}(", function.name);

            let mut first = true;

            for arg in arguments {
                if first {
                    first = false;
                } else {
                    output.push_str(", ");
                }
                let arg = expr(arg, env);
                output.push_str(&arg);
            }
            output.push(')');
            output
        }
    }
}

fn typ_to_string(typ: &Type, env: &mut Env) -> String {
    match typ {
        Type::Integer => String::from("int"),
        Type::Variable(var) => panic!("codegen with unresolved generic {:?}", var),
        Type::Function(args, result) => {
            let name = env.fresh_name("fp");
            let result = typ_to_string(&result, env);
            write!(&mut env.preamble, "typedef {} (*{})(", result, &name);
            env.preamble.comma_list(args, |gen, arg| {
                write!(gen, "{:?}", arg);
            });

            write!(&mut env.preamble, ");");
            env.preamble.newline();
            name
        }
    }
}
