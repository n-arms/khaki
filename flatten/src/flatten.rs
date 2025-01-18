use std::collections::HashMap;

use ir::hir::{Argument, EnumCase, Expr, Identifier, MatchCase, Program, Type};

use crate::env::Env;

#[derive(Default)]
struct Subst {
    generics: HashMap<Identifier, Type>,
}

impl Subst {
    pub fn get(&self, name: &Identifier) -> Type {
        self.generics
            .get(name)
            .unwrap_or_else(|| panic!("unspecified generic {:?}", name))
            .clone()
    }

    fn zip<'a>(
        names: impl IntoIterator<Item = &'a Identifier>,
        types: impl IntoIterator<Item = &'a Type>,
    ) -> Self {
        Self {
            generics: names
                .into_iter()
                .cloned()
                .zip(types.into_iter().cloned())
                .collect(),
        }
    }
}

pub fn flatten_program(program: &Program) -> Program {
    let mut env = Env::new(program);

    for def in program.enums.values() {
        if def.generics.is_empty() {
            flatten_enum(&def.name, Vec::new(), &mut env);
        }
    }

    for func in &program.functions {
        if func.generics.is_empty() {
            flatten_function(&func.name, Vec::new(), &mut env);
        }
    }

    env.build()
}

/// ensure that `generics` have been flattened
fn flatten_enum(name: &Identifier, generics: Vec<Type>, env: &mut Env) -> Identifier {
    let def = env.get_enum(name).clone();
    let subst = Subst::zip(&def.generics, &generics);

    if let Some(name) = env.generated(name.clone(), generics.clone()) {
        return name;
    }

    let new_name = if generics.is_empty() {
        name.clone()
    } else {
        env.fresh(&name.name)
    };

    env.generate(name.clone(), generics.clone(), new_name.clone());

    let cases = def
        .cases
        .iter()
        .map(|case| EnumCase::new(case.name.clone(), flatten_typ(&case.typ, &subst, env)))
        .collect();

    env.def(new_name.clone(), cases);

    new_name
}

fn flatten_typ(typ: &Type, generics: &Subst, env: &mut Env) -> Type {
    match typ {
        Type::Integer => Type::Integer,
        Type::Variable(name) => generics.get(name),
        Type::Function(args, res, set) => Type::Function(
            args.iter()
                .map(|arg| flatten_typ(arg, generics, env))
                .collect(),
            Box::new(flatten_typ(res, generics, env)),
            set.clone(),
        ),
        Type::Tuple(elems) => Type::Tuple(
            elems
                .iter()
                .map(|elem| flatten_typ(elem, generics, env))
                .collect(),
        ),
        Type::Constructor(name, args) => {
            let flat_args = args
                .iter()
                .map(|arg| flatten_typ(arg, generics, env))
                .collect();

            let flat_name = flatten_enum(name, flat_args, env);

            Type::Constructor(flat_name, Vec::new())
        }
    }
}

/// ensure that `generics` have been flattened
fn flatten_function(name: &Identifier, generics: Vec<Type>, env: &mut Env) -> Identifier {
    let function = env.get_function(name).clone();
    let subst = Subst::zip(&function.generics, &generics);

    if let Some(name) = env.generated(name.clone(), generics.clone()) {
        return name;
    }

    let new_name = if generics.is_empty() {
        name.clone()
    } else {
        env.fresh(&name.name)
    };

    env.generate(name.clone(), generics.clone(), new_name.clone());

    let arguments = function
        .arguments
        .iter()
        .map(|arg| Argument {
            name: arg.name.clone(),
            typ: flatten_typ(&arg.typ, &subst, env),
        })
        .collect();

    let result = flatten_typ(&function.result, &subst, env);

    let body = flatten_expr(&function.body, &subst, env);

    env.function(new_name.clone(), arguments, result, body);

    new_name
}

fn flatten_expr(expr: &Expr, generics: &Subst, env: &mut Env) -> Expr {
    match expr {
        Expr::Integer(_) => expr.clone(),
        Expr::Variable {
            name,
            generics: func_generics,
            typ,
        } => {
            let flat_typ = flatten_typ(typ, generics, env);
            if func_generics.is_empty() {
                Expr::Variable {
                    name: name.clone(),
                    generics: Vec::new(),
                    typ: flat_typ,
                }
            } else {
                let flat_generics = func_generics
                    .iter()
                    .map(|typ| flatten_typ(typ, generics, env))
                    .collect();
                Expr::Variable {
                    name: flatten_function(name, flat_generics, env),
                    generics: Vec::new(),
                    typ: flat_typ,
                }
            }
        }
        Expr::FunctionCall {
            function,
            set,
            arguments,
        } => Expr::FunctionCall {
            function: Box::new(flatten_expr(function, generics, env)),
            set: set.clone(),
            arguments: arguments
                .iter()
                .map(|arg| flatten_expr(arg, generics, env))
                .collect(),
        },
        Expr::Function {
            captures,
            arguments,
            result,
            body,
            set,
            name,
        } => {
            let flat_caps = captures
                .iter()
                .map(|cap| Argument {
                    name: cap.name.clone(),
                    typ: flatten_typ(&cap.typ, generics, env),
                })
                .collect();
            let flat_args = arguments
                .iter()
                .map(|cap| Argument {
                    name: cap.name.clone(),
                    typ: flatten_typ(&cap.typ, generics, env),
                })
                .collect();
            let flat_res = flatten_typ(result, generics, env);
            let flat_body = flatten_expr(body, generics, env);
            Expr::Function {
                captures: flat_caps,
                arguments: flat_args,
                result: flat_res,
                body: Box::new(flat_body),
                set: set.clone(),
                name: name.clone(),
            }
        }
        Expr::Tuple(elems) => Expr::Tuple(
            elems
                .iter()
                .map(|elem| flatten_expr(elem, generics, env))
                .collect(),
        ),
        Expr::TupleAccess(tuple, field) => {
            Expr::TupleAccess(Box::new(flatten_expr(tuple, generics, env)), *field)
        }
        Expr::Enum {
            typ,
            tag,
            generics: enum_generics,
            argument,
        } => {
            let flat_generics = enum_generics
                .iter()
                .map(|typ| flatten_typ(typ, generics, env))
                .collect();
            let new_typ = flatten_enum(typ, flat_generics, env);
            let flat_arg = flatten_expr(argument, generics, env);

            Expr::Enum {
                typ: new_typ,
                tag: tag.clone(),
                generics: Vec::new(),
                argument: Box::new(flat_arg),
            }
        }
        Expr::Match { head, cases } => {
            let flat_head = flatten_expr(head, generics, env);
            let flat_cases = cases
                .iter()
                .map(|case| MatchCase {
                    variant: case.variant.clone(),
                    binding: case.binding.clone(),
                    binding_type: flatten_typ(&case.binding_type, generics, env),
                    body: flatten_expr(&case.body, generics, env),
                })
                .collect();

            Expr::Match {
                head: Box::new(flat_head),
                cases: flat_cases,
            }
        }
        Expr::Let {
            name,
            typ,
            value,
            rest,
        } => Expr::Let {
            name: name.clone(),
            typ: flatten_typ(typ, generics, env),
            value: Box::new(flatten_expr(value, generics, env)),
            rest: Box::new(flatten_expr(rest, generics, env)),
        },
    }
}
