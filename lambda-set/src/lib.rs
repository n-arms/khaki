use std::{cell::RefCell, rc::Rc};

use im::HashMap;
use ir::{
    base,
    parsed::{Argument, Enum, Expr, Function, Identifier, Program, Type},
};
use lower::{lower_program, Lower};
use patch::{patch_function, Patcher};
use unify::infer_function;
use union_find::UnionFind;

mod lower;
mod patch;
mod unify;
mod union_find;

pub fn program(prog: &mut Program) -> base::Program {
    let mut env = HashMap::new();

    let mut uf = UnionFind::new();

    for func in prog.functions.iter() {
        env.insert(func.name.clone(), func.typ(uf.token()));
    }

    for func in prog.functions.iter_mut() {
        infer_function(func, env.clone(), &mut uf, &prog.enums);
    }

    let mut patcher = Patcher::new(uf);

    for func in &mut prog.functions {
        patch_function(func, &mut patcher);
    }

    let mut lower = Lower::new(patcher.pools, patcher.lambdas);

    lower_program(prog, &mut lower)
}
