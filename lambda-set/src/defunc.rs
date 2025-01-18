use std::collections::HashMap;

use ir::hir::{Argument, Enum, Function, Identifier, Program, Type};

use crate::pool::LambdaSetPool;

struct Env {
    names: usize,
}

impl Env {
    pub fn new() -> Self {
        Self { names: 0 }
    }
    pub fn fresh(&mut self, prefix: &str) -> Identifier {
        let token = self.names;
        self.names += 1;
        Identifier::from(format!("{prefix}_{token}"))
    }
}

pub fn defunc_program(program: &mut Program, pools: &HashMap<Identifier, LambdaSetPool>) {
    let mut env = Env::new();
    for pool in pools.values() {
        let (def, func) = pool_caller(pool, &mut env);
        program.enums.insert(def.name.clone(), def);
        program.functions.push(func);
    }
}

/// generate the `call_closure_x` function that takes a lambda set enum and dynamically dispatches the function call to the correct function.
fn pool_caller(pool: &LambdaSetPool, env: &mut Env) -> (Enum, Function) {
    let enum_name = env.fresh("Closure");
    let mut arguments = pool.arguments;
    arguments.push(Argument {
        name: Identifier::from(String::from("closure")),
        typ: Type::Constructor(enum_name.clone(), Vec::new()),
    });
    let def = Enum {
        name: enum_name,
        generics: pool.generics,
        cases: todo!(),
    };
    let func = Function {
        name: Identifier::from(format!("call_closure_{}", pool.set.token)),
        arguments,
        generics: todo!(),
        result: todo!(),
        body: todo!(),
    }
    (def, func)
}
