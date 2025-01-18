use ir::{
    base::{Expr, Function, MatchCase, Program, Stmt, Type, Variable},
    hir,
};

use crate::{
    builder::{Builder, Env},
    sort,
};

pub fn lower_program(program: &hir::Program) -> Program {
    let mut env = Env::default();

    for def in program.enums.values() {
        lower_enum(def, &mut env);
    }

    let functions = program
        .functions
        .iter()
        .map(|func| lower_function(func, &mut env))
        .collect();

    let definitions = env.definitions();
    Program {
        definitions: sort::sort_definitions(definitions),
        functions,
    }
}

fn lower_enum(def: &hir::Enum, env: &mut Env) {
    let cases = def
        .cases
        .iter()
        .map(|(name, typ)| (name.clone(), lower_typ(typ, env)))
        .collect();
    env.enum_def(def.name.clone(), cases)
}

fn lower_function(func: &hir::Function, env: &mut Env) -> Function {
    let arguments = func
        .arguments
        .iter()
        .map(|arg| Variable::new(arg.name.clone(), lower_typ(&arg.typ, env)))
        .collect();
    let mut builder = Builder::default();
    let result = lower_expr(&func.body, &mut builder, env);
    let body = builder.build(result);
    Function {
        name: func.name.clone(),
        arguments,
        body,
    }
}

fn lower_typ(typ: &hir::Type, env: &mut Env) -> Type {
    match typ {
        hir::Type::Integer => Type::Integer,
        hir::Type::Variable(_) | hir::Type::Function(_, _, _) => {
            unreachable!("complex type {:?} should have been lowered already", typ)
        }
        hir::Type::Tuple(fields) => {
            let low_fields: Vec<_> = fields.iter().map(|field| lower_typ(field, env)).collect();
            Type::Constructor(env.tuple_name(&low_fields))
        }
        hir::Type::Constructor(name, args) => {
            assert!(args.is_empty());
            Type::Constructor(name.clone())
        }
    }
}

fn lower_expr(expr: &hir::Expr, block: &mut Builder, env: &mut Env) -> Variable {
    let result_typ = lower_typ(&expr.typ(), env);
    let result = env.fresh_var(result_typ);
    let value = match expr {
        hir::Expr::Integer(int) => Expr::Integer(*int),
        hir::Expr::Variable {
            name,
            generics,
            typ,
        } => {
            assert!(generics.is_empty());
            return Variable::new(name.clone(), lower_typ(typ, env));
        }
        hir::Expr::FunctionCall {
            function,
            arguments,
            ..
        } => {
            let hir::Expr::Variable { name, generics, .. } = function.as_ref() else {
                unreachable!("function {:?} was not a variable", function);
            };
            assert!(generics.is_empty());
            let low_args = arguments
                .iter()
                .map(|arg| lower_expr(arg, block, env))
                .collect();
            Expr::DirectCall {
                function: name.clone(),
                arguments: low_args,
            }
        }
        hir::Expr::Function { .. } => unreachable!(
            "annonymous function {:?} should have already been lowered",
            expr
        ),
        hir::Expr::Tuple(elems) => {
            let low_elems = elems
                .iter()
                .map(|arg| lower_expr(arg, block, env))
                .collect();
            Expr::Tuple(low_elems)
        }
        hir::Expr::TupleAccess(tuple, field) => {
            let low_tuple = lower_expr(&tuple, block, env);
            Expr::TupleAccess(low_tuple, *field)
        }
        hir::Expr::Enum {
            typ,
            tag,
            generics,
            argument,
        } => {
            assert!(generics.is_empty());
            let low_arg = lower_expr(&argument, block, env);
            Expr::Enum {
                typ: typ.clone(),
                tag: tag.clone(),
                argument: low_arg,
            }
        }
        hir::Expr::Match { head, cases } => {
            let low_head = lower_expr(&head, block, env);
            let low_cases = cases
                .iter()
                .map(|case| {
                    let binding =
                        Variable::new(case.binding.clone(), lower_typ(&case.binding_type, env));
                    let mut inner = Builder::default();
                    let result = lower_expr(&case.body, &mut inner, env);
                    let body = inner.build(result);
                    MatchCase {
                        variant: case.variant.clone(),
                        binding,
                        body,
                    }
                })
                .collect();
            Expr::Match {
                head: low_head,
                cases: low_cases,
            }
        }
        hir::Expr::Let {
            name,
            typ,
            value,
            rest,
        } => {
            let low_typ = lower_typ(typ, env);
            let low_value = lower_expr(&value, block, env);
            block.stmt(Stmt {
                var: Variable::new(name.clone(), low_typ),
                value: Expr::Variable(low_value),
            });
            return lower_expr(&rest, block, env);
        }
    };
    block.stmt(Stmt {
        var: result.clone(),
        value,
    });
    result
}
