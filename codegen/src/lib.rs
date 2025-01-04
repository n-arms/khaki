use generator::{generate, Env, Generator};
use ir::parsed::{Expr, Function, Type};

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
    generate!(&mut env.main, "{} {:?}(", result, &func.name);
    let args: Vec<_> = func
        .arguments
        .iter()
        .map(|arg| typ_to_string(&arg.typ, env))
        .collect();
    env.main.comma_list(args, |gen, arg| {
        generate!(gen, "{}", arg);
    });
    generate!(&mut env.main, ");");
    env.main.newline();
}

fn function(func: &Function, env: &mut Env) {
    let result = typ_to_string(&func.result, env);
    generate!(&mut env.main, "{} {:?}(", result, &func.name);
    let args: Vec<_> = func
        .arguments
        .iter()
        .map(|arg| typ_to_string(&arg.typ, env))
        .collect();
    env.main
        .comma_list(args.into_iter().zip(&func.arguments), |gen, (typ, arg)| {
            generate!(gen, "{} {}", typ, arg.name.name);
        });
    generate!(&mut env.main, ") {{");
    env.main.newline();
    env.main.inc();

    let result = expr(&func.body, env);

    generate!(&mut env.main, "return {result};");
    env.main.newline();

    env.main.dec();
    generate!(&mut env.main, "}}");
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
        Expr::Function {
            captures,
            arguments,
            result,
            body,
            set,
            name,
        } => todo!(),
        Expr::Tuple(elems) => {
            let typ = elems.iter().map(|e| e.typ()).collect::<Vec<_>>();
            let name = env.tuple_name(&typ);

            let mut output = format!("(struct {}) {{", name.name);

            let mut first = true;
            for elem in elems {
                if first {
                    first = false;
                } else {
                    output.push_str(",");
                }
                output.push(' ');
                output.push_str(&expr(elem, env));
            }
            output.push_str(" }");
            output
        }
        Expr::TupleAccess(tuple, field) => {
            format!("({}).field{}", expr(&tuple, env), field)
        }
    }
}

fn typ_to_string(typ: &Type, env: &mut Env) -> String {
    match typ {
        Type::Integer => String::from("int"),
        Type::Variable(var) => panic!("codegen with unresolved generic {:?}", var),
        Type::Function(args, result, set) => {
            let name = env.fresh_name("fp");
            let result = typ_to_string(&result, env);
            generate!(&mut env.preamble, "typedef {} (*{})(", result, &name);
            env.preamble.comma_list(args, |gen, arg| {
                generate!(gen, "{:?}", arg);
            });

            generate!(&mut env.preamble, ");");
            env.preamble.newline();
            name
        }
        Type::Tuple(elems) => {
            let name = env.tuple_name(elems);
            format!("struct {}", name.name)
        }
    }
}
