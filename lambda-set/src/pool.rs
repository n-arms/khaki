use ir::hir::{Argument, Identifier, LambdaSet, Type};

#[derive(Clone, Debug)]
pub struct LambdaSetPool {
    pub set: LambdaSet,
    pub lambdas: Vec<Lambda>,
    pub arguments: Vec<Argument>,
    pub result: Type,
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub name: Identifier,
    pub captures: Vec<Argument>,
}
