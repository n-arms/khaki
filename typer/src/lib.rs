mod infer;
mod patch;
mod substitute;

use std::result;

pub use infer::infer_program;
use ir::{
    hir,
    parsed::{self, Enum, Identifier, Pattern, Span, Type},
    union_find::UnionFind,
};
use patch::{patch_program, Patcher};

#[derive(Debug)]
pub enum Error {
    DuplicateEnumDefinition(Identifier, Identifier),
    UnknownEnum(Identifier),
    UnknownVariable(Identifier),
    DuplicateFunctionDefinition(Identifier, Identifier),
    InappropriateTuplePattern(Vec<Pattern>, Span, Type),
    RigidTypeMismatch(Identifier, Identifier, Span),
    ConstructorMismatch(Identifier, Vec<Type>, Identifier, Vec<Type>, Span),
    TypeMismatch(Type, Type),
    UndefinedEnumVariant(Enum, Identifier),
}

pub type Result<T> = result::Result<T, Error>;

pub fn type_program(mut program: parsed::Program, lambda_sets: usize) -> Result<hir::Program> {
    let union_find = infer_program(&mut program, lambda_sets)?;
    let mut patcher = Patcher::new(union_find);
    Ok(patch_program(&program, &mut patcher))
}
