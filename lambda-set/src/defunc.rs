use std::collections::{HashMap, HashSet};

use ir::hir::{Argument, Enum, Expr, Function, Identifier, LambdaSet, MatchCase, Program, Type};

use crate::pool::LambdaSetPool;

struct Env {
    names: usize,
    set_names: HashMap<LambdaSet, Identifier>,
    functions: HashSet<Identifier>,
}

impl Env {
    pub fn new(functions: impl IntoIterator<Item = Identifier>) -> Self {
        Self {
            names: 0,
            set_names: HashMap::new(),
            functions: functions.into_iter().collect(),
        }
    }

    pub fn fresh(&mut self, prefix: &str) -> Identifier {
        let token = self.names;
        self.names += 1;
        Identifier::from(format!("{prefix}_{token}"))
    }

    pub fn set_name(&mut self, set: &LambdaSet) -> Identifier {
        if let Some(name) = self.set_names.get(set) {
            name.clone()
        } else {
            let name = self.fresh("Closure");
            self.set_names.insert(set.clone(), name.clone());
            name
        }
    }

    fn is_function(&self, name: &Identifier) -> bool {
        self.functions.contains(name)
    }
}

pub fn defunc_program(program: &mut Program, pools: &[LambdaSetPool]) {
    let mut env = Env::new(program.functions.iter().map(|func| func.name.clone()));
    for (_, def) in &mut program.enums {
        defunc_enum(def, &mut env);
    }
    for function in &mut program.functions {
        defunc_function(function, &mut env);
    }
    for pool in pools {
        let (def, func) = pool_caller(pool, &mut env);
        program.enums.insert(def.name.clone(), def);
        program.functions.push(func);
    }
}

fn defunc_enum(def: &mut Enum, env: &mut Env) {
    for (_, case) in &mut def.cases {
        *case = defunc_typ(case, env);
    }
}

/// generate the `call_closure_x` function that takes a lambda set enum and dynamically dispatches the function call to the correct function.
fn pool_caller(pool: &LambdaSetPool, env: &mut Env) -> (Enum, Function) {
    let enum_name = env.set_name(&pool.set);
    let mut arguments: Vec<_> = pool
        .arguments
        .iter()
        .map(|arg| Argument {
            name: arg.name.clone(),
            typ: defunc_typ(&arg.typ, env),
        })
        .collect();
    let closure_arg = Identifier::from(String::from("closure"));
    let closure_typ = Type::Constructor(enum_name.clone(), Vec::new());
    arguments.push(Argument {
        name: closure_arg.clone(),
        typ: closure_typ.clone(),
    });
    let cases = pool
        .lambdas
        .iter()
        .map(|lambda| {
            let caps = lambda
                .captures
                .iter()
                .map(|cap| defunc_typ(&cap.typ, env))
                .collect();
            (lambda.name.clone(), Type::Tuple(caps))
        })
        .collect();
    let def = Enum {
        name: enum_name,
        generics: Vec::new(),
        cases,
    };

    let match_cases = pool
        .lambdas
        .iter()
        .map(|lambda| {
            let caps: Vec<_> = lambda
                .captures
                .iter()
                .map(|cap| defunc_typ(&cap.typ, env))
                .collect();
            let binding = env.fresh("captures");
            let (mut call_args, mut arg_typs): (Vec<_>, Vec<_>) = pool
                .arguments
                .iter()
                .map(|arg| {
                    let typ = defunc_typ(&arg.typ, env);
                    (
                        Expr::Variable {
                            name: arg.name.clone(),
                            generics: Vec::new(),
                            typ: typ.clone(),
                        },
                        typ,
                    )
                })
                .collect();
            for (i, cap) in lambda.captures.iter().enumerate() {
                let typ = defunc_typ(&cap.typ, env);
                arg_typs.push(typ.clone());
                call_args.push(Expr::TupleAccess(
                    Box::new(Expr::Variable {
                        name: binding.clone(),
                        generics: Vec::new(),
                        typ: Type::Tuple(caps.clone()),
                    }),
                    i,
                ))
            }
            let typ = Type::Function(
                arg_typs,
                Box::new(defunc_typ(&pool.result, env)),
                pool.set.clone(),
            );
            let body = Expr::FunctionCall {
                function: Box::new(Expr::Variable {
                    name: lambda.name.clone(),
                    generics: Vec::new(),
                    typ,
                }),
                set: pool.set.clone(),
                arguments: call_args,
            };
            MatchCase {
                variant: lambda.name.clone(),
                binding: binding.clone(),
                binding_type: Type::Tuple(caps),
                body,
            }
        })
        .collect();

    let body = Expr::Match {
        head: Box::new(Expr::Variable {
            name: closure_arg,
            generics: Vec::new(),
            typ: closure_typ,
        }),
        cases: match_cases,
    };

    let func = Function {
        name: call_function_name(&pool.set),
        arguments,
        generics: Vec::new(),
        result: defunc_typ(&pool.result, env),
        body,
    };
    (def, func)
}

fn call_function_name(set: &LambdaSet) -> Identifier {
    Identifier::from(format!("call_closure_{}", set.token))
}

fn defunc_typ(typ: &Type, env: &mut Env) -> Type {
    match typ {
        Type::Constructor(..) | Type::Integer | Type::Variable(_) => typ.clone(),
        Type::Function(_, _, set) => Type::Constructor(env.set_name(set), Vec::new()),
        Type::Tuple(elems) => Type::Tuple(elems.iter().map(|elem| defunc_typ(elem, env)).collect()),
    }
}

fn defunc_function(function: &mut Function, env: &mut Env) {
    for arg in function.arguments.iter_mut() {
        arg.typ = defunc_typ(&arg.typ, env);
    }

    function.result = defunc_typ(&function.result, env);
    defunc_expr(&mut function.body, env);
}

fn defunc_expr(expr: &mut Expr, env: &mut Env) {
    match expr {
        Expr::Integer(_) => {}
        Expr::Variable { name, typ, .. } => {
            if env.is_function(name) {
                let Type::Function(_, _, set) = typ else {
                    unreachable!()
                };

                *expr = Expr::Enum {
                    typ: env.set_name(&set),
                    tag: name.clone(),
                    generics: Vec::new(),
                    argument: Box::new(Expr::Tuple(Vec::new())),
                };
            } else {
                *typ = defunc_typ(typ, env)
            }
        }
        Expr::FunctionCall {
            function,
            set,
            arguments,
        } => {
            let typ = {
                let Type::Function(mut args, res, _) = function.typ() else {
                    unreachable!()
                };
                for arg in args.iter_mut() {
                    *arg = defunc_typ(arg, env);
                }
                args.push(Type::Variable(env.set_name(set)));
                Type::Function(args, Box::new(defunc_typ(&res, env)), set.clone())
            };
            defunc_expr(function.as_mut(), env);
            for arg in arguments.iter_mut() {
                defunc_expr(arg, env);
            }
            arguments.push(*function.clone());
            *function.as_mut() = Expr::Variable {
                name: call_function_name(set),
                generics: Vec::new(),
                typ,
            };
        }
        Expr::Function {
            captures,
            set,
            name,
            ..
        } => {
            let captures_tuple = Expr::Tuple(
                captures
                    .iter()
                    .map(|cap| Expr::Variable {
                        name: cap.name.clone(),
                        generics: Vec::new(),
                        typ: cap.typ.clone(),
                    })
                    .collect(),
            );

            *expr = Expr::Enum {
                typ: env.set_name(&set),
                tag: name.clone(),
                generics: Vec::new(),
                argument: Box::new(captures_tuple),
            };
        }
        Expr::Tuple(elems) => {
            for elem in elems {
                defunc_expr(elem, env);
            }
        }
        Expr::TupleAccess(tuple, _) => defunc_expr(tuple.as_mut(), env),
        Expr::Enum { argument, .. } => defunc_expr(argument.as_mut(), env),
        Expr::Match { head, cases } => {
            defunc_expr(head.as_mut(), env);
            for case in cases {
                case.binding_type = defunc_typ(&case.binding_type, env);
                defunc_expr(&mut case.body, env);
            }
        }
        Expr::Let {
            typ, value, rest, ..
        } => {
            *typ = defunc_typ(typ, env);
            defunc_expr(value.as_mut(), env);
            defunc_expr(rest.as_mut(), env);
        }
    }
}
