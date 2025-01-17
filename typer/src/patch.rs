use ir::{
    hir::{self, LambdaSet},
    parsed::{Function, Program},
    union_find::UnionFind,
};

pub fn patch_program(program: &Program, lambda_sets: &mut UnionFind<LambdaSet>) -> hir::Program {
    let functions = program
        .functions
        .iter()
        .map(|func| patch_function(func, lambda_sets))
        .collect();
    let enums = program
        .enums
        .iter()
        .map(|def| patch_enum(def, lambda_sets))
        .collect();
    hir::Program { functions, enums }
}

fn patch_function(func: &Function, lambda_sets: &mut UnionFind<LambdaSet>) -> hir::Function {}
