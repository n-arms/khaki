use crate::union_find::UnionFind;
use im::HashMap;
use ir::parsed::{Argument, Expr, Function, Identifier, Type};

pub(crate) struct Patcher {
    pub uf: UnionFind,
    /// 1-to-1 for lambda id and lambda body
    pub lambdas: HashMap<Identifier, Lambda>,
    // 1-to-many mapping from the root lambda id to all of the relevant lambdas
    pub pools: HashMap<usize, Vec<Identifier>>,
    pub functions: HashMap<Identifier, Lambda>,
}

#[derive(Clone, Debug)]
pub(crate) struct Lambda {
    pub captures: Vec<Argument>,
    pub arguments: Vec<Argument>,
    pub result: Type,
    pub body: Expr,
    pub name: Identifier,
}

impl Patcher {
    pub(crate) fn new(mut uf: UnionFind, functions: HashMap<Identifier, Lambda>) -> Self {
        let patched_functions = functions
            .into_iter()
            .map(|(name, mut func)| {
                for arg in func.captures.iter_mut().chain(func.arguments.iter_mut()) {
                    patch_type_uf(&mut arg.typ, &mut uf);
                }
                patch_type_uf(&mut func.result, &mut uf);
                (name, func)
            })
            .collect();
        Self {
            uf,
            lambdas: HashMap::new(),
            pools: HashMap::new(),
            functions: patched_functions,
        }
    }

    fn root(&mut self, token: usize) -> usize {
        self.uf.root(token)
    }

    fn append_pool(&mut self, mut root: usize, name: Identifier) {
        println!("append pool {root} {}", name.name);
        root = self.root(root);
        let pool = self.pools.entry(root).or_default();
        if !pool.contains(&name) {
            pool.push(name);
        }
    }

    fn append_lambda(&mut self, lambda: Lambda) {
        println!("append lambda {}", lambda.name.name);
        self.lambdas.insert(lambda.name.clone(), lambda);
    }

    fn function_name(&mut self, name: &Identifier, typ: &Type) {
        let Some(lambda) = self.functions.get_mut(name) else {
            return;
        };
        let mut new_lambda = lambda.clone();
        for arg in new_lambda.arguments.iter_mut() {
            patch_type(&mut arg.typ, self);
        }
        patch_type(&mut new_lambda.result, self);
        patch_expr(&mut new_lambda.body, self);
        self.functions.insert(name.clone(), new_lambda.clone());
        let lambda = new_lambda;
        self.lambdas.insert(name.clone(), lambda.clone());
        let Type::Function(_, _, set) = typ else {
            unreachable!()
        };
        self.append_pool(set.token, name.clone());
    }
}

fn patch_type_uf(to_patch: &mut Type, patcher: &mut UnionFind) {
    match to_patch {
        Type::Integer => {}
        Type::Variable(_) => unreachable!(),
        Type::Function(args, result, set) => {
            for arg in args {
                patch_type_uf(arg, patcher);
            }
            set.token = patcher.root(set.token);
            patch_type_uf(result.as_mut(), patcher);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                patch_type_uf(elem, patcher);
            }
        }
        Type::Constructor(_) => {}
    }
}

fn patch_type(to_patch: &mut Type, patcher: &mut Patcher) {
    patch_type_uf(to_patch, &mut patcher.uf)
}

fn patch_expr(to_patch: &mut Expr, patcher: &mut Patcher) {
    println!("{:?}", to_patch);
    match to_patch {
        Expr::Integer(_) => {}
        Expr::Variable { name, typ, .. } => {
            if let Some(typ) = typ {
                patch_type(typ, patcher);
            }
            patcher.function_name(name, typ.as_ref().unwrap());
        }
        Expr::FunctionCall {
            function,
            set,
            arguments,
        } => {
            patch_expr(function.as_mut(), patcher);
            set.token = patcher.root(set.token);
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
            println!("visiting lambda {}", name.name);
            for arg in captures.iter_mut().chain(arguments.iter_mut()) {
                patch_type(&mut arg.typ, patcher);
            }

            patch_type(result, patcher);
            patch_expr(body.as_mut(), patcher);
            set.token = patcher.root(set.token);
            patcher.append_pool(set.token, name.clone());

            patcher.append_lambda(Lambda {
                captures: captures.clone(),
                arguments: arguments.clone(),
                result: result.clone(),
                body: body.as_ref().clone(),
                name: name.clone(),
            });
        }
        Expr::Tuple(elems) => {
            for elem in elems {
                patch_expr(elem, patcher);
            }
        }
        Expr::TupleAccess(tuple, _) => {
            patch_expr(tuple.as_mut(), patcher);
        }
        Expr::Enum { argument, .. } => {
            patch_expr(argument.as_mut(), patcher);
        }
        Expr::Match { head, cases } => {
            patch_expr(head.as_mut(), patcher);
            for case in cases {
                patch_expr(&mut case.body, patcher);
                patch_type(case.binding_type.as_mut().unwrap(), patcher);
            }
        }
    }
}

pub(crate) fn patch_function(to_patch: &mut Function, patcher: &mut Patcher) {
    for arg in &mut to_patch.arguments {
        patch_type(&mut arg.typ, patcher);
    }
    patch_type(&mut to_patch.result, patcher);
    patch_expr(&mut to_patch.body, patcher);
}
