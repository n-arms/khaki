use im::HashMap;
use ir::{base, parsed::Program};
use lower::{lower_program, Lower};
use patch::{patch_function, Lambda, Patcher};
use unify::infer_function;
use union_find::UnionFind;

mod lower;
mod order;
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
        infer_function(func, env.clone(), &mut uf, &mut prog.enums);
        println!("{:?}", func);
    }

    println!("generated union find {:?}", uf);

    let functions = prog
        .functions
        .iter()
        .cloned()
        .map(|func| {
            (
                func.name.clone(),
                Lambda {
                    captures: Vec::new(),
                    arguments: func.arguments,
                    result: func.result,
                    body: func.body,
                    name: func.name,
                },
            )
        })
        .collect();

    println!("generated predfined functions {:?}", functions);

    let mut patcher = Patcher::new(uf, functions);

    for func in &mut prog.functions {
        patch_function(func, &mut patcher);
    }

    let function_set = prog
        .functions
        .iter()
        .map(|func| func.name.clone())
        .collect();

    let mut lower = Lower::new(patcher.pools, patcher.lambdas, function_set);

    lower_program(prog, &mut lower)
}
