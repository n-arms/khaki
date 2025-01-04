use std::{cell::RefCell, rc::Rc};

use im::HashMap;
use ir::parsed::{Expr, Function, Identifier, Type};
use union_find::UnionFind;

mod union_find;

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
        _ => panic!(),
    }
}

pub fn program(functions: &mut [Function]) {
    let mut env = HashMap::new();

    let mut uf = UnionFind::new();

    for func in functions.iter() {
        env.insert(func.name.clone(), func.typ(uf.token()));
    }

    for func in functions.iter_mut() {
        infer_function(func, env.clone(), &mut uf);
    }

    let mut patcher = Patcher {
        uf,
        pools: HashMap::new(),
        names: functions.len(),
    };

    for func in functions {
        patch_function(func, &mut patcher);
    }
}

struct Patcher {
    uf: UnionFind,
    pools: HashMap<usize, Rc<RefCell<Vec<Identifier>>>>,
    names: usize,
}

impl Patcher {
    pub fn get_pool(&mut self, token: usize) -> Rc<RefCell<Vec<Identifier>>> {
        self.pools.entry(self.uf.root(token)).or_default().clone()
    }

    pub fn name(&mut self) -> Identifier {
        let name = self.names;
        self.names += 1;
        Identifier::from(format!("Closure_{name}"))
    }
}

fn patch_type(to_patch: &mut Type, patcher: &mut Patcher) {
    match to_patch {
        Type::Integer => {}
        Type::Variable(_) => unreachable!(),
        Type::Function(args, result, set) => {
            for arg in args {
                patch_type(arg, patcher);
            }
            patch_type(result.as_mut(), patcher);
            set.pool = patcher.get_pool(set.token);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                patch_type(elem, patcher);
            }
        }
    }
}

fn patch_expr(to_patch: &mut Expr, patcher: &mut Patcher) {
    match to_patch {
        Expr::Integer(_) => {}
        Expr::Variable { name, typ } => {
            if let Some(typ) = typ {
                patch_type(typ, patcher);
            }
        }
        Expr::FunctionCall {
            function,
            set,
            generics,
            arguments,
        } => {
            set.pool = patcher.get_pool(set.token);
            for arg in arguments {
                patch_expr(arg, patcher);
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
            for arg in captures.iter_mut().chain(arguments) {
                patch_type(&mut arg.typ, patcher);
            }

            set.pool = patcher.get_pool(set.token);
            patch_type(result, patcher);
            patch_expr(body.as_mut(), patcher);
            *name = patcher.name();
            set.pool.as_ref().borrow_mut().push(name.clone());
        }
        Expr::Tuple(elems) => {
            for elem in elems {
                patch_expr(elem, patcher);
            }
        }
    }
}

fn patch_function(to_patch: &mut Function, patcher: &mut Patcher) {
    for arg in &mut to_patch.arguments {
        patch_type(&mut arg.typ, patcher);
    }
    patch_type(&mut to_patch.result, patcher);
    patch_expr(&mut to_patch.body, patcher);
}

fn infer_function(to_infer: &mut Function, mut env: HashMap<Identifier, Type>, uf: &mut UnionFind) {
    for arg in to_infer.arguments.iter().cloned() {
        env.insert(arg.name, arg.typ);
    }
    let body_typ = infer_expr(&mut to_infer.body, env, uf);
    union_type(&body_typ, &to_infer.result, uf);
}

fn infer_expr(to_infer: &mut Expr, env: HashMap<Identifier, Type>, uf: &mut UnionFind) -> Type {
    match to_infer {
        Expr::Integer(_) => Type::Integer,
        Expr::Variable { name, typ } => {
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
            let Type::Function(env_args, env_res, env_set) = env[function].clone() else {
                panic!()
            };

            for (arg, env_arg) in arguments.iter_mut().zip(env_args.iter()) {
                let ty = infer_expr(arg, env.clone(), uf);
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
            name,
        } => {
            set.token = uf.token();
            let mut inner = env.clone();
            for arg in arguments.iter().chain(captures.iter()) {
                inner.insert(arg.name.clone(), arg.typ.clone());
            }
            let inferred_result = infer_expr(body.as_mut(), inner, uf);
            union_type(&result, &inferred_result, uf);

            let arg_types = arguments.iter().map(|arg| arg.typ.clone()).collect();

            Type::Function(arg_types, Box::new(inferred_result), set.clone())
        }
        Expr::Tuple(elems) => {
            let typs = elems
                .iter_mut()
                .map(|elem| infer_expr(elem, env.clone(), uf))
                .collect();
            Type::Tuple(typs)
        }
    }
}
