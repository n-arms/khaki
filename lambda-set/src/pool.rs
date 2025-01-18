use ir::hir::{Argument, Identifier, LambdaSet, Type};

#[derive(Clone, Debug)]
pub struct LambdaSetPool {
    pub set: LambdaSet,
    pub lambdas: Vec<Lambda>,
    pub argument_generics: Vec<Identifier>,
    pub capture_generics: Vec<Identifier>,
    pub arguments: Vec<Argument>,
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub name: Identifier,
    // the generics in the corresponding pool that are used by this lambda are in generics[pool_generics_start..pool_generics_end]
    pub capture_generics_start: usize,
    pub capture_generics_end: usize,
    pub captures: Vec<Argument>,
}
