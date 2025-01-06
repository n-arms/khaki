use std::{cell::RefCell, rc::Rc};

use crate::{patch::Lambda, union_find::UnionFind};
use im::{HashMap, HashSet};
use ir::base::{self, Stmt};
use ir::parsed::{Argument, Enum, Expr, Function, Identifier, LambdaSet, Program, Type};

#[derive(Clone)]
pub(crate) struct LambdaStruct {
    lambdas: Vec<Lambda>,
    name: Identifier,
}

pub(crate) struct Lower {
    pools: HashMap<usize, LambdaStruct>,
    tuples: HashMap<Vec<Type>, Identifier>,
    // list of user defined functions
    functions: HashSet<Identifier>,
    names: usize,
}

impl Lower {
    pub(crate) fn new(
        pools: HashMap<usize, Vec<Identifier>>,
        lambdas: HashMap<Identifier, Lambda>,
        functions: HashSet<Identifier>,
    ) -> Self {
        let pools = pools
            .into_iter()
            .map(|(pool, ids)| {
                (
                    pool,
                    LambdaStruct {
                        lambdas: ids.iter().map(|id| lambdas[id].clone()).collect(),
                        name: Identifier::from(format!("closure_{}", pool)),
                    },
                )
            })
            .collect();

        Self {
            pools,
            tuples: HashMap::new(),
            names: 0,
            functions,
        }
    }

    fn lambda_set_type(&self, set: &LambdaSet) -> base::Type {
        let lambda_struct = &self.pools[&set.token];
        base::Type::Constructor(lambda_struct.name.clone())
    }

    fn fresh_name(&mut self, prefix: &str) -> Identifier {
        let name = self.names;
        self.names += 1;
        Identifier::from(format!("{}_{}", prefix, name))
    }

    fn tuple_name(&mut self, tuple: &[Type]) -> Identifier {
        if let Some(name) = self.tuples.get(tuple) {
            return name.clone();
        } else {
            let name = self.fresh_name("tuple");
            self.tuples.insert(tuple.to_vec(), name.clone());
            name
        }
    }

    fn is_function(&self, name: &Identifier) -> bool {
        self.functions.contains(name)
    }
}

pub(crate) fn lower_program(to_lower: &Program, lower: &mut Lower) -> base::Program {
    let mut functions = Vec::new();
    let mut enums = Vec::new();

    let pools: Vec<_> = lower.pools.iter().map(|(k, v)| (*k, v.clone())).collect();
    for (token, lambda_struct) in pools {
        let cases = lambda_struct
            .lambdas
            .iter()
            .map(|lambda| {
                (
                    lambda.name.clone(),
                    base::Type::Constructor(
                        lower.tuple_name(
                            lambda
                                .captures
                                .iter()
                                .map(|cap| cap.typ.clone())
                                .collect::<Vec<_>>()
                                .as_slice(),
                        ),
                    ),
                )
            })
            .collect();
        enums.push(base::Enum {
            name: lambda_struct.name.clone(),
            cases,
        });
        {
            let func_name = lower.fresh_name("var");
            let mut arguments: Vec<_> = lambda_struct.lambdas[0]
                .arguments
                .iter()
                .map(|arg| base::Argument {
                    name: arg.name.clone(),
                    typ: lower_type(&arg.typ, lower),
                })
                .collect();
            arguments.push(base::Argument {
                name: func_name.clone(),
                typ: base::Type::Constructor(lambda_struct.name.clone()),
            });
            let mut body = Vec::new();
            let result = lower.fresh_name("var");
            let cases = lambda_struct
                .lambdas
                .iter()
                .map(|lambda| {
                    let mut body = Vec::new();
                    let payload_var = lower.fresh_name("var");

                    let mut call_args: Vec<_> = lambda
                        .arguments
                        .iter()
                        .map(|arg| arg.name.clone())
                        .collect();

                    for (i, cap) in lambda.captures.iter().enumerate() {
                        let var = lower.fresh_name("var");
                        call_args.push(var.clone());
                        body.push(Stmt::Let {
                            name: var,
                            typ: lower_type(&cap.typ, lower),
                            value: base::Expr::TupleAccess(payload_var.clone(), i),
                        });
                    }

                    body.push(Stmt::Let {
                        name: result.clone(),
                        typ: lower_type(&lambda.result, lower),
                        value: base::Expr::DirectCall {
                            function: lambda.name.clone(),
                            arguments: call_args,
                        },
                    });

                    base::MatchCase {
                        variant: lambda.name.clone(),
                        binding: payload_var.clone(),
                        body,
                    }
                })
                .collect();
            body.push(Stmt::Match {
                head: func_name,
                cases,
            });
            let typ = lower_type(&lambda_struct.lambdas[0].result, lower);
            functions.push(base::Function {
                name: Identifier::from(format!("call_closure_{}", token)),
                arguments,
                typ,
                body,
                result,
            });
        }
        {
            for lambda in &lambda_struct.lambdas {
                let arguments = lambda
                    .captures
                    .iter()
                    .chain(lambda.arguments.iter())
                    .map(|arg| base::Argument {
                        name: arg.name.clone(),
                        typ: lower_type(&arg.typ, lower),
                    })
                    .collect();
                let mut body = Vec::new();
                let result = lower_expr(&lambda.body, &mut body, lower);
                functions.push(base::Function {
                    name: lambda.name.clone(),
                    arguments,
                    typ: lower_type(&lambda.result, lower),
                    body,
                    result,
                });
            }
        }
    }

    for func in &to_lower.functions {
        functions.push(lower_function(func, lower));
    }

    for (_, enum_def) in &to_lower.enums {
        enums.push(lower_enum(enum_def, lower));
    }

    base::Program { enums, functions }
}

fn lower_enum(to_lower: &Enum, lower: &mut Lower) -> base::Enum {
    let cases = to_lower
        .cases
        .iter()
        .map(|(name, typ)| (name.clone(), lower_type(typ, lower)))
        .collect();
    base::Enum {
        name: to_lower.name.clone(),
        cases,
    }
}

fn lower_function(to_lower: &Function, lower: &mut Lower) -> base::Function {
    let mut body = Vec::new();
    let arguments = to_lower
        .arguments
        .iter()
        .map(|arg| base::Argument {
            name: arg.name.clone(),
            typ: lower_type(&arg.typ, lower),
        })
        .collect();
    let result = lower_expr(&to_lower.body, &mut body, lower);
    base::Function {
        name: to_lower.name.clone(),
        arguments,
        typ: lower_type(&to_lower.body.typ(), lower),
        body,
        result,
    }
}

fn lower_type(to_lower: &Type, lower: &mut Lower) -> base::Type {
    match to_lower {
        Type::Integer => base::Type::Integer,
        Type::Variable(_) => unreachable!(),
        Type::Function(_, _, set) => lower.lambda_set_type(set),
        Type::Tuple(elems) => base::Type::Constructor(lower.tuple_name(elems)),
        Type::Constructor(name) => base::Type::Constructor(name.clone()),
    }
}

fn lower_expr(to_lower: &Expr, stmts: &mut Vec<Stmt>, lower: &mut Lower) -> Identifier {
    let result = lower.fresh_name("var");
    match to_lower {
        Expr::Integer(int) => {
            stmts.push(Stmt::Let {
                name: result.clone(),
                typ: base::Type::Integer,
                value: base::Expr::Integer(*int),
            });
        }
        Expr::Variable { name, typ, .. } => {
            if lower.is_function(name) {
                let Type::Function(_, _, set) = typ.as_ref().unwrap() else {
                    unreachable!()
                };
                let typ = Identifier::from(format!("closure_{}", set.token));
                let payload_typ = lower.tuple_name(&[]);
                let payload = base::Expr::Tuple(Vec::new());
                let payload_var = lower.fresh_name("var");
                stmts.push(Stmt::Let {
                    name: payload_var.clone(),
                    typ: base::Type::Constructor(payload_typ),
                    value: payload,
                });
                stmts.push(Stmt::Let {
                    name: result.clone(),
                    typ: base::Type::Constructor(typ.clone()),
                    value: base::Expr::Enum {
                        typ,
                        tag: name.clone(),
                        argument: payload_var,
                    },
                });
            } else {
                return name.clone();
            }
        }
        Expr::FunctionCall {
            function,
            set,
            arguments,
        } => {
            let lowered_func = lower_expr(&function, stmts, lower);
            let lowered_args: Vec<_> = arguments
                .iter()
                .map(|arg| lower_expr(arg, stmts, lower))
                .collect();
            let func_name = Identifier::from(format!("call_closure_{}", set.token));
            let mut call_set_args = lowered_args;
            call_set_args.push(lowered_func);
            stmts.push(Stmt::Let {
                name: result.clone(),
                typ: lower_type(&to_lower.typ(), lower),
                value: base::Expr::DirectCall {
                    function: func_name,
                    arguments: call_set_args,
                },
            });
        }
        Expr::Function {
            captures,
            set,
            name,
            ..
        } => {
            let typ = Identifier::from(format!("closure_{}", set.token));
            let payload_typ = lower.tuple_name(
                captures
                    .iter()
                    .map(|cap| cap.typ.clone())
                    .collect::<Vec<_>>()
                    .as_slice(),
            );
            let payload = base::Expr::Tuple(captures.iter().map(|cap| cap.name.clone()).collect());
            let payload_var = lower.fresh_name("var");
            stmts.push(Stmt::Let {
                name: payload_var.clone(),
                typ: base::Type::Constructor(payload_typ),
                value: payload,
            });
            stmts.push(Stmt::Let {
                name: result.clone(),
                typ: base::Type::Constructor(typ.clone()),
                value: base::Expr::Enum {
                    typ,
                    tag: name.clone(),
                    argument: payload_var,
                },
            });
        }
        Expr::Tuple(elems) => {
            let payload_typ = lower.tuple_name(
                elems
                    .iter()
                    .map(|elem| elem.typ())
                    .collect::<Vec<_>>()
                    .as_slice(),
            );
            let payload = base::Expr::Tuple(
                elems
                    .iter()
                    .map(|elem| lower_expr(elem, stmts, lower))
                    .collect(),
            );
            stmts.push(Stmt::Let {
                name: result.clone(),
                typ: base::Type::Constructor(payload_typ),
                value: payload,
            });
        }
        Expr::TupleAccess(tuple, field) => {
            let lowered_tuple = lower_expr(tuple.as_ref(), stmts, lower);
            stmts.push(Stmt::Let {
                name: result.clone(),
                typ: lower_type(&to_lower.typ(), lower),
                value: base::Expr::TupleAccess(lowered_tuple, *field),
            });
        }
        Expr::Enum { typ, tag, argument } => {
            let lowered_arg = lower_expr(argument.as_ref(), stmts, lower);
            stmts.push(Stmt::Let {
                name: result.clone(),
                typ: base::Type::Constructor(typ.clone()),
                value: base::Expr::Enum {
                    typ: typ.clone(),
                    tag: tag.clone(),
                    argument: lowered_arg,
                },
            });
        }
        Expr::Match { head, cases } => {
            let head_expr = lower_expr(head.as_ref(), stmts, lower);
            let lowered_cases = cases
                .iter()
                .map(|case| {
                    let mut body = Vec::new();
                    let case_result = lower_expr(&case.body, &mut body, lower);
                    body.push(Stmt::Let {
                        name: result.clone(),
                        typ: lower_type(&to_lower.typ(), lower),
                        value: base::Expr::Variable(case_result),
                    });
                    base::MatchCase {
                        variant: case.variant.clone(),
                        binding: case.binding.clone(),
                        body,
                    }
                })
                .collect();
            stmts.push(Stmt::Match {
                head: head_expr,
                cases: lowered_cases,
            });
        }
    }
    result
}
