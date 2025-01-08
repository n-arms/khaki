use crate::order;
use crate::patch::Lambda;
use im::{HashMap, HashSet};
use ir::base::{self, Stmt, Variable};
use ir::parsed::{Enum, Expr, Function, Identifier, LambdaSet, Program, Type};

#[derive(Clone, Debug)]
pub(crate) struct LambdaStruct {
    lambdas: Vec<Lambda>,
    name: Identifier,
}

pub(crate) struct Lower {
    pools: HashMap<usize, LambdaStruct>,
    tuples: HashMap<Vec<Type>, Identifier>,
    // list of user defined functions
    functions: HashSet<Identifier>,
    structs: Vec<base::Struct>,
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
            functions,
            structs: Vec::new(),
            names: 0,
        }
    }

    fn lambda_set_type(&self, set: &LambdaSet) -> base::Type {
        let lambda_struct = self
            .pools
            .get(&set.token)
            .unwrap_or_else(|| panic!("unknown set {}", set.token));
        base::Type::Constructor(lambda_struct.name.clone())
    }

    fn fresh_name(&mut self, prefix: &str) -> Identifier {
        let name = self.names;
        self.names += 1;
        Identifier::from(format!("{}_{}", prefix, name))
    }

    fn fresh_var(&mut self, typ: base::Type) -> Variable {
        Variable::new(self.fresh_name("var"), typ)
    }

    fn tuple_name(&mut self, tuple: &[Type]) -> Identifier {
        if let Some(name) = self.tuples.get(tuple) {
            name.clone()
        } else {
            let name = self.fresh_name("tuple");
            self.tuples.insert(tuple.to_vec(), name.clone());
            let fields = tuple.iter().map(|elem| lower_type(elem, self)).collect();
            self.structs.push(base::Struct {
                name: name.clone(),
                fields,
            });
            name
        }
    }

    fn is_function(&self, name: &Identifier) -> bool {
        self.functions.contains(name)
    }
}

#[derive(Default)]
struct BlockBuilder {
    stmts: Vec<Stmt>,
}

impl BlockBuilder {
    fn stmt(&mut self, var: Variable, value: base::Expr) -> &mut Self {
        self.stmts.push(Stmt { var, value });
        self
    }

    fn finish(self, result: Variable) -> base::Block {
        base::Block {
            stmts: self.stmts,
            result,
        }
    }
}

pub(crate) fn lower_program(to_lower: &Program, lower: &mut Lower) -> base::Program {
    let mut functions = Vec::new();
    let mut enums = Vec::new();

    println!("lowering program with start");
    for func in &to_lower.functions {
        println!("{:?}", func);
    }

    let pools: Vec<_> = lower.pools.iter().map(|(k, v)| (*k, v.clone())).collect();
    println!("{:?}", pools);
    println!("lowering program with end");
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
            let func_name = lower.fresh_var(base::Type::Constructor(lambda_struct.name.clone()));
            let mut arguments: Vec<_> = lambda_struct.lambdas[0]
                .arguments
                .iter()
                .map(|arg| base::Variable {
                    name: arg.name.clone(),
                    typ: lower_type(&arg.typ, lower),
                })
                .collect();
            arguments.push(func_name.clone());
            let mut body = BlockBuilder::default();
            let result_typ = lower_type(&lambda_struct.lambdas[0].result, lower);
            let result = lower.fresh_var(result_typ);
            let cases = lambda_struct
                .lambdas
                .iter()
                .map(|lambda| {
                    let mut body = BlockBuilder::default();
                    let payload_typ = base::Type::Constructor(
                        lower.tuple_name(
                            lambda
                                .captures
                                .iter()
                                .map(|cap| cap.typ.clone())
                                .collect::<Vec<_>>()
                                .as_slice(),
                        ),
                    );
                    let payload_var = lower.fresh_var(payload_typ);

                    let mut call_args: Vec<_> = lambda
                        .arguments
                        .iter()
                        .map(|arg| Variable::new(arg.name.clone(), lower_type(&arg.typ, lower)))
                        .collect();

                    for (i, cap) in lambda.captures.iter().enumerate() {
                        let var_typ = lower_type(&cap.typ, lower);
                        let var = lower.fresh_var(var_typ);
                        call_args.push(var.clone());
                        body.stmt(var, base::Expr::TupleAccess(payload_var.clone(), i));
                    }

                    let call_var = lower.fresh_var(result.typ.clone());

                    body.stmt(
                        call_var.clone(),
                        base::Expr::DirectCall {
                            function: lambda.name.clone(),
                            arguments: call_args,
                        },
                    );

                    base::MatchCase {
                        variant: lambda.name.clone(),
                        binding: payload_var.clone(),
                        body: body.finish(call_var),
                    }
                })
                .collect();
            body.stmt(
                result.clone(),
                base::Expr::Match {
                    head: func_name,
                    cases,
                },
            );
            functions.push(base::Function {
                name: Identifier::from(format!("call_closure_{}", token)),
                arguments,
                body: body.finish(result),
            });
        }
        {
            for lambda in &lambda_struct.lambdas {
                let arguments = lambda
                    .arguments
                    .iter()
                    .chain(lambda.captures.iter())
                    .map(|arg| base::Variable {
                        name: arg.name.clone(),
                        typ: lower_type(&arg.typ, lower),
                    })
                    .collect();
                let mut body = BlockBuilder::default();
                let result = lower_expr(&lambda.body, &mut body, lower);
                functions.push(base::Function {
                    name: lambda.name.clone(),
                    arguments,
                    body: body.finish(result),
                });
            }
        }
    }

    for func in &to_lower.functions {
        functions.push(lower_function(func, lower));
    }

    remove_duplicates(&mut functions);

    for enum_def in to_lower.enums.values() {
        enums.push(lower_enum(enum_def, lower));
    }

    let mut definitions = Vec::new();

    for def in enums {
        definitions.push(base::Definition::Enum(def));
    }

    for def in lower.structs.clone() {
        definitions.push(base::Definition::Struct(def));
    }

    definitions = order::sort_definitions(definitions);

    base::Program {
        functions,
        definitions,
    }
}

fn remove_duplicates(functions: &mut Vec<base::Function>) {
    let mut seen = HashSet::new();
    functions.retain(|func| {
        if seen.contains(&func.name) {
            false
        } else {
            seen.insert(func.name.clone());
            true
        }
    });
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
    let mut body = BlockBuilder::default();
    let arguments = to_lower
        .arguments
        .iter()
        .map(|arg| base::Variable {
            name: arg.name.clone(),
            typ: lower_type(&arg.typ, lower),
        })
        .collect();
    let result = lower_expr(&to_lower.body, &mut body, lower);
    base::Function {
        name: to_lower.name.clone(),
        arguments,
        body: body.finish(result),
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

fn lower_expr(to_lower: &Expr, stmts: &mut BlockBuilder, lower: &mut Lower) -> Variable {
    let result_typ = lower_type(&to_lower.typ(), lower);
    let result = lower.fresh_var(result_typ);
    match to_lower {
        Expr::Integer(int) => {
            stmts.stmt(result.clone(), base::Expr::Integer(*int));
        }
        Expr::Variable { name, typ, .. } => {
            if lower.is_function(name) {
                let Type::Function(_, _, set) = typ.as_ref().unwrap() else {
                    unreachable!()
                };
                let typ = Identifier::from(format!("closure_{}", set.token));
                let payload_typ = lower.tuple_name(&[]);
                let payload = base::Expr::Tuple(Vec::new());
                let payload_var = lower.fresh_var(base::Type::Constructor(payload_typ));
                stmts.stmt(payload_var.clone(), payload);
                stmts.stmt(
                    result.clone(),
                    base::Expr::Enum {
                        typ,
                        tag: name.clone(),
                        argument: payload_var,
                    },
                );
            } else {
                return Variable::new(name.clone(), lower_type(typ.as_ref().unwrap(), lower));
            }
        }
        Expr::FunctionCall {
            function,
            set,
            arguments,
        } => {
            let lowered_func = lower_expr(function, stmts, lower);
            let lowered_args: Vec<_> = arguments
                .iter()
                .map(|arg| lower_expr(arg, stmts, lower))
                .collect();
            let func_name = Identifier::from(format!("call_closure_{}", set.token));
            let mut call_set_args = lowered_args;
            call_set_args.push(lowered_func);
            stmts.stmt(
                result.clone(),
                base::Expr::DirectCall {
                    function: func_name,
                    arguments: call_set_args,
                },
            );
        }
        Expr::Function {
            captures,
            set,
            name,
            ..
        } => {
            let typ = Identifier::from(format!("closure_{}", set.token));
            let (payload_vars, payload_typs): (Vec<_>, Vec<_>) = captures
                .iter()
                .map(|cap| {
                    let typ = lower_type(&cap.typ, lower);
                    (Variable::new(cap.name.clone(), typ), cap.typ.clone())
                })
                .unzip();
            let payload_typ = lower.tuple_name(&payload_typs);
            let payload = base::Expr::Tuple(payload_vars);
            let payload_var = lower.fresh_var(base::Type::Constructor(payload_typ));
            stmts.stmt(payload_var.clone(), payload);
            stmts.stmt(
                result.clone(),
                base::Expr::Enum {
                    typ,
                    tag: name.clone(),
                    argument: payload_var,
                },
            );
        }
        Expr::Tuple(elems) => {
            let payload = base::Expr::Tuple(
                elems
                    .iter()
                    .map(|elem| lower_expr(elem, stmts, lower))
                    .collect(),
            );
            stmts.stmt(result.clone(), payload);
        }
        Expr::TupleAccess(tuple, field) => {
            let lowered_tuple = lower_expr(tuple.as_ref(), stmts, lower);
            stmts.stmt(
                result.clone(),
                base::Expr::TupleAccess(lowered_tuple, *field),
            );
        }
        Expr::Enum { typ, tag, argument } => {
            let lowered_arg = lower_expr(argument.as_ref(), stmts, lower);
            stmts.stmt(
                result.clone(),
                base::Expr::Enum {
                    typ: typ.clone(),
                    tag: tag.clone(),
                    argument: lowered_arg,
                },
            );
        }
        Expr::Match { head, cases } => {
            let head_expr = lower_expr(head.as_ref(), stmts, lower);
            let lowered_cases = cases
                .iter()
                .map(|case| {
                    let mut body = BlockBuilder::default();
                    let case_result = lower_expr(&case.body, &mut body, lower);
                    let binding = Variable::new(
                        case.binding.clone(),
                        lower_type(case.binding_type.as_ref().unwrap(), lower),
                    );
                    base::MatchCase {
                        variant: case.variant.clone(),
                        binding,
                        body: body.finish(case_result),
                    }
                })
                .collect();
            stmts.stmt(
                result.clone(),
                base::Expr::Match {
                    head: head_expr,
                    cases: lowered_cases,
                },
            );
        }
    }
    result
}
