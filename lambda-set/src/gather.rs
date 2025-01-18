use std::collections::HashMap;

use ir::hir::{Argument, Expr, Function, Identifier, LambdaSet, Program, Type};

use crate::pool::{Lambda, LambdaSetPool};

#[derive(Default)]
struct Pools {
    pools: HashMap<LambdaSet, LambdaSetPool>,
}

impl Pools {
    fn lambda(
        &mut self,
        set: &LambdaSet,
        name: Identifier,
        captures: Vec<Argument>,
        arguments: &[Argument],
        result: &Type,
    ) {
        let pool = self
            .pools
            .entry(set.clone())
            .or_insert_with(move || LambdaSetPool {
                set: set.clone(),
                lambdas: Vec::new(),
                arguments: arguments.to_vec(),
                result: result.clone(),
            });
        pool.lambdas.push(Lambda { name, captures });
    }
}

pub fn gather_program(program: &mut Program) -> Vec<LambdaSetPool> {
    let functions = program
        .functions
        .iter()
        .map(|func| (func.name.clone(), func.clone()))
        .collect();

    let mut lambda_funcs = Vec::new();

    let mut pools = Pools::default();

    for func in &program.functions {
        gather_function(func, &mut pools, &functions, &mut lambda_funcs);
    }

    program.functions.extend(lambda_funcs);

    pools.pools.values().cloned().collect()
}

fn gather_function(
    function: &Function,
    pools: &mut Pools,
    functions: &HashMap<Identifier, Function>,
    lambdas: &mut Vec<Function>,
) {
    gather_expr(&function.body, pools, functions, lambdas)
}

fn gather_expr(
    expr: &Expr,
    pools: &mut Pools,
    functions: &HashMap<Identifier, Function>,
    lambdas: &mut Vec<Function>,
) {
    match expr {
        Expr::Integer(_) => {}
        Expr::Variable { name, typ, .. } => {
            if let Some(function) = functions.get(name) {
                let Type::Function(_, _, set) = typ else {
                    unreachable!("expected function type, got {:?}", typ)
                };
                pools.lambda(
                    set,
                    name.clone(),
                    Vec::new(),
                    &function.arguments,
                    &function.result,
                )
            }
        }
        Expr::FunctionCall {
            function,
            arguments,
            ..
        } => {
            gather_expr(&function, pools, functions, lambdas);
            for arg in arguments {
                gather_expr(arg, pools, functions, lambdas)
            }
        }
        Expr::Function {
            captures,
            arguments,
            result,
            body,
            set,
            name,
        } => {
            let lambda_args: Vec<_> = arguments.iter().chain(captures).cloned().collect();

            let function = Function {
                name: name.clone(),
                arguments: lambda_args,
                generics: Vec::new(),
                result: result.clone(),
                body: body.as_ref().clone(),
            };
            lambdas.push(function);
            pools.lambda(set, name.clone(), captures.clone(), arguments, result);
            gather_expr(&body, pools, functions, lambdas);
        }
        Expr::Tuple(elems) => {
            for elem in elems {
                gather_expr(elem, pools, functions, lambdas)
            }
        }
        Expr::TupleAccess(tuple, _) => gather_expr(&tuple, pools, functions, lambdas),
        Expr::Enum { argument, .. } => gather_expr(&argument, pools, functions, lambdas),
        Expr::Match { head, cases } => {
            gather_expr(&head, pools, functions, lambdas);
            for case in cases {
                gather_expr(&case.body, pools, functions, lambdas);
            }
        }
        Expr::Let { value, rest, .. } => {
            gather_expr(&value, pools, functions, lambdas);
            gather_expr(&rest, pools, functions, lambdas);
        }
    }
}
