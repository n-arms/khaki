use crate::union_find::UnionFind;
use im::HashMap;
use ir::parsed::{Argument, Expr, Function, Identifier, Type};

pub(crate) struct Patcher {
    pub uf: UnionFind,
    /// 1-to-1 for lambda id and lambda body
    pub lambdas: HashMap<Identifier, Lambda>,
    // 1-to-many mapping from the root lambda id to all of the relevant lambdas
    pub pools: HashMap<usize, Vec<Identifier>>,
    names: usize,
}

#[derive(Clone)]
pub(crate) struct Lambda {
    pub captures: Vec<Argument>,
    pub arguments: Vec<Argument>,
    pub result: Type,
    pub body: Expr,
    pub name: Identifier,
}

impl Patcher {
    pub(crate) fn new(uf: UnionFind) -> Self {
        Self {
            uf,
            lambdas: HashMap::new(),
            pools: HashMap::new(),
            names: 0,
        }
    }

    fn name(&mut self) -> Identifier {
        let name = self.names;
        self.names += 1;
        Identifier::from(format!("Closure_{name}"))
    }

    fn root(&mut self, token: usize) -> usize {
        self.uf.root(token)
    }

    fn append_pool(&mut self, mut root: usize, name: Identifier) {
        root = self.root(root);
        self.pools.entry(root).or_default().push(name);
    }

    fn append_lambda(&mut self, lambda: Lambda) {
        self.lambdas.insert(lambda.name.clone(), lambda);
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
            set.token = patcher.root(set.token);
            patch_type(result.as_mut(), patcher);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                patch_type(elem, patcher);
            }
        }
        Type::Constructor(_) => {}
    }
}

fn patch_expr(to_patch: &mut Expr, patcher: &mut Patcher) {
    match to_patch {
        Expr::Integer(_) => {}
        Expr::Variable { typ, .. } => {
            if let Some(typ) = typ {
                patch_type(typ, patcher);
            }
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
            for arg in captures.iter_mut().chain(arguments.iter_mut()) {
                patch_type(&mut arg.typ, patcher);
            }

            patch_type(result, patcher);
            patch_expr(body.as_mut(), patcher);
            *name = patcher.name();
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
