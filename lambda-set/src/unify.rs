use std::collections;

use crate::union_find::UnionFind;
use im::HashMap;
use ir::parsed::{Enum, Expr, Function, Identifier, Type};

fn union_type(ty1: &Type, ty2: &Type, uf: &mut UnionFind) {
    use Type::*;
    match (ty1, ty2) {
        (Integer, Integer) => {}
        (Variable(id1), Variable(id2)) if id1 == id2 => {}
        (Function(args1, res1, set1), Function(args2, res2, set2)) => {
            uf.merge(set1.token, set2.token);
            assert_eq!(args1.len(), args2.len());

            for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                union_type(arg1, arg2, uf);
            }

            union_type(&res1, &res2, uf);
        }
        (Tuple(elems1), Tuple(elems2)) => {
            assert_eq!(elems1.len(), elems2.len());
            for (e1, e2) in elems1.iter().zip(elems2.iter()) {
                union_type(e1, e2, uf);
            }
        }
        (Constructor(name1), Constructor(name2)) if name1 == name2 => {}
        _ => panic!(),
    }
}

pub(crate) fn infer_function(
    to_infer: &mut Function,
    mut env: HashMap<Identifier, Type>,
    uf: &mut UnionFind,
    enums: &collections::HashMap<Identifier, Enum>,
) {
    for arg in to_infer.arguments.iter().cloned() {
        env.insert(arg.name, arg.typ);
    }
    let body_typ = infer_expr(&mut to_infer.body, env, uf, enums);
    union_type(&body_typ, &to_infer.result, uf);
}

fn infer_expr(
    to_infer: &mut Expr,
    env: HashMap<Identifier, Type>,
    uf: &mut UnionFind,
    enums: &collections::HashMap<Identifier, Enum>,
) -> Type {
    match to_infer {
        Expr::Integer(_) => Type::Integer,
        Expr::Variable { name, typ, .. } => {
            let env_typ = env[&name].clone();
            if let Some(old_typ) = typ {
                union_type(old_typ, &env_typ, uf);
            } else {
                *typ = Some(env_typ);
            }
            typ.clone().unwrap()
        }
        Expr::FunctionCall {
            function,
            set,
            arguments,
            ..
        } => {
            let Type::Function(env_args, env_res, env_set) =
                infer_expr(function.as_mut(), env.clone(), uf, enums)
            else {
                panic!()
            };

            for (arg, env_arg) in arguments.iter_mut().zip(env_args.iter()) {
                let ty = infer_expr(arg, env.clone(), uf, enums);
                union_type(&ty, env_arg, uf);
            }

            set.token = uf.token();
            uf.merge(set.token, env_set.token);

            env_res.as_ref().clone()
        }
        Expr::Function {
            captures,
            arguments,
            result,
            body,
            set,
            ..
        } => {
            set.token = uf.token();
            let mut inner = env.clone();
            for arg in arguments.iter().chain(captures.iter()) {
                inner.insert(arg.name.clone(), arg.typ.clone());
            }
            let inferred_result = infer_expr(body.as_mut(), inner, uf, enums);
            union_type(&result, &inferred_result, uf);

            let arg_types = arguments.iter().map(|arg| arg.typ.clone()).collect();

            Type::Function(arg_types, Box::new(inferred_result), set.clone())
        }
        Expr::Tuple(elems) => {
            let typs = elems
                .iter_mut()
                .map(|elem| infer_expr(elem, env.clone(), uf, enums))
                .collect();
            Type::Tuple(typs)
        }
        Expr::TupleAccess(tuple, field) => {
            let tuple_typ = infer_expr(tuple.as_mut(), env, uf, enums);

            if let Type::Tuple(elems) = tuple_typ {
                elems[*field].clone()
            } else {
                unreachable!()
            }
        }
        Expr::Enum { typ, tag, argument } => {
            let arg_typ = infer_expr(argument.as_mut(), env, uf, enums);

            // TODO: check type of arg against enum def

            Type::Constructor(typ.clone())
        }
        Expr::Match { head, cases } => {
            let head_typ = infer_expr(head.as_mut(), env.clone(), uf, enums);
            // TODO: check head type against the patterns being matched

            let Type::Constructor(enum_name) = head_typ else {
                panic!()
            };
            let enum_def = &enums[&enum_name];
            let case_typs: Vec<Type> = cases
                .iter_mut()
                .map(|case| {
                    let mut inner = env.clone();
                    let typ = enum_def.variant_type(&case.variant);
                    inner.insert(case.binding.clone(), typ.clone());
                    infer_expr(&mut case.body, inner, uf, enums)
                })
                .collect();
            for window in case_typs.windows(2) {
                union_type(&window[0], &window[1], uf);
            }
            case_typs[0].clone()
        }
    }
}
