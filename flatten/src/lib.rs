use std::{cell::Cell, rc::Rc};

use im::HashMap;
use ir::hir::{Argument, Expr, Function, Identifier, MatchCase, Program, Type};

#[derive(Clone, Default)]
pub struct Env {
    functions: HashMap<Identifier, Function>,
    names: Rc<Cell<usize>>,
}

impl Env {
    pub fn fresh_from(&self, base: Identifier) -> Identifier {
        let tag = self.names.take();
        self.names.set(tag + 1);
        Identifier {
            name: format!("{}_{tag}", base.name),
        }
    }
}

pub fn program(prog: Program) -> Program {
    let mut bank = Vec::new();
    let env = Env {
        functions: prog
            .functions
            .iter()
            .map(|func| (func.name.clone(), func.clone()))
            .collect(),
        ..Default::default()
    };
    for func in prog.functions {
        if func.generics.is_empty() {
            let name = func.name.clone();
            function(func, name, HashMap::new(), env.clone(), &mut bank);
        }
    }
    Program {
        functions: bank,
        ..prog
    }
}

fn function(
    to_flat: Function,
    name: Identifier,
    generics: HashMap<Identifier, Type>,
    env: Env,
    bank: &mut Vec<Function>,
) {
    let flat_args = to_flat
        .arguments
        .into_iter()
        .map(|arg| Argument {
            name: arg.name,
            typ: replace_type(arg.typ, generics.clone()),
        })
        .collect();
    let flat_body = expr(replace_expr(to_flat.body, generics.clone()), env, bank);
    bank.push(Function {
        name,
        arguments: flat_args,
        generics: Vec::new(),
        body: flat_body,
        result: replace_type(to_flat.result, generics),
    });
}

fn expr(to_flat: Expr, env: Env, bank: &mut Vec<Function>) -> Expr {
    match to_flat {
        int @ Expr::Integer(_) => int,
        Expr::Variable {
            name,
            generics,
            typ,
        } => {
            if generics.is_empty() {
                return Expr::Variable {
                    name,
                    generics,
                    typ,
                };
            }
            let func = env.functions.get(&name).unwrap().clone();
            let flat_func = env.fresh_from(func.name.clone());
            let generics = func.generics.iter().cloned().zip(generics).collect();
            function(func, flat_func.clone(), generics, env.clone(), bank);

            Expr::Variable {
                name: flat_func,
                generics: Vec::new(),
                typ,
            }
        }
        Expr::FunctionCall {
            function,
            arguments,
            set,
        } => {
            let flat_args = arguments
                .into_iter()
                .map(|arg| expr(arg, env.clone(), bank))
                .collect();
            let flat_func = expr(*function, env, bank);
            Expr::FunctionCall {
                function: Box::new(flat_func),
                arguments: flat_args,
                set,
            }
        }
        Expr::Function {
            captures,
            arguments,
            result,
            set,
            body,
            name,
        } => Expr::Function {
            captures,
            arguments,
            result,
            body: Box::new(expr(*body, env, bank)),
            set,
            name,
        },
        Expr::Tuple(elems) => Expr::Tuple(
            elems
                .into_iter()
                .map(|elem| expr(elem, env.clone(), bank))
                .collect(),
        ),
        Expr::TupleAccess(tuple, field) => {
            Expr::TupleAccess(Box::new(expr(*tuple, env, bank)), field)
        }
        Expr::Enum {
            typ,
            tag,
            generics,
            argument,
        } => {
            assert!(generics.is_empty());
            Expr::Enum {
                typ,
                tag,
                generics,
                argument: Box::new(expr(*argument, env, bank)),
            }
        }
        Expr::Match { head, cases } => {
            let cases = cases
                .into_iter()
                .map(|case| MatchCase {
                    body: expr(case.body, env.clone(), bank),
                    ..case
                })
                .collect();
            Expr::Match {
                cases,
                head: Box::new(expr(*head, env, bank)),
            }
        }
    }
}

fn replace_expr(expr: Expr, generics: HashMap<Identifier, Type>) -> Expr {
    match expr {
        int @ Expr::Integer(_) => int,
        Expr::Variable {
            name,
            typ,
            generics: call_generics,
        } => Expr::Variable {
            name,
            generics: call_generics
                .into_iter()
                .map(|generic| replace_type(generic, generics.clone()))
                .collect(),
            typ: replace_type(typ, generics),
        },
        Expr::FunctionCall {
            function,
            arguments,
            set,
        } => Expr::FunctionCall {
            function,
            arguments: arguments
                .into_iter()
                .map(|arg| replace_expr(arg, generics.clone()))
                .collect(),
            set,
        },
        Expr::Function {
            captures,
            arguments,
            result,
            body,
            set,
            name,
        } => {
            let captures = captures
                .into_iter()
                .map(|arg| Argument {
                    name: arg.name,
                    typ: replace_type(arg.typ, generics.clone()),
                })
                .collect();
            let arguments = arguments
                .into_iter()
                .map(|arg| Argument {
                    name: arg.name,
                    typ: replace_type(arg.typ, generics.clone()),
                })
                .collect();
            let result = replace_type(result, generics.clone());
            let body = replace_expr(*body, generics);
            Expr::Function {
                captures,
                arguments,
                result,
                body: Box::new(body),
                set,
                name,
            }
        }
        Expr::Tuple(elems) => Expr::Tuple(
            elems
                .into_iter()
                .map(|elem| replace_expr(elem, generics.clone()))
                .collect(),
        ),
        Expr::TupleAccess(tuple, field) => {
            Expr::TupleAccess(Box::new(replace_expr(*tuple, generics)), field)
        }
        Expr::Enum {
            typ,
            tag,
            generics: enum_generics,
            argument,
        } => {
            assert!(enum_generics.is_empty());
            Expr::Enum {
                typ,
                tag,
                generics: enum_generics,
                argument: Box::new(replace_expr(*argument, generics)),
            }
        }
        Expr::Match { head, cases } => Expr::Match {
            cases: cases
                .into_iter()
                .map(|case| MatchCase {
                    body: replace_expr(case.body, generics.clone()),
                    ..case
                })
                .collect(),
            head: Box::new(replace_expr(*head, generics)),
        },
    }
}

fn replace_type(typ: Type, generics: HashMap<Identifier, Type>) -> Type {
    match typ {
        typ @ Type::Integer => typ,
        Type::Variable(generic) => {
            if let Some(typ) = generics.get(&generic) {
                typ.clone()
            } else {
                Type::Variable(generic)
            }
        }
        Type::Function(arguments, function, set) => Type::Function(
            arguments
                .into_iter()
                .map(|arg| replace_type(arg, generics.clone()))
                .collect(),
            Box::new(replace_type(*function, generics)),
            set,
        ),
        Type::Tuple(elems) => Type::Tuple(
            elems
                .into_iter()
                .map(|elem| replace_type(elem, generics.clone()))
                .collect(),
        ),
        typ @ Type::Constructor(..) => typ,
    }
}
