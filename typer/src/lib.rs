mod infer;
mod patch;
mod substitute;

use std::result;

pub use infer::infer_program;
use ir::parsed::{self, Enum, Pattern, Span, Type};

pub enum Error {
    DuplicateEnumDefinition(parsed::Identifier, parsed::Identifier),
    UnknownEnum(parsed::Identifier),
    UnknownVariable(parsed::Identifier),
    DuplicateFunctionDefinition(parsed::Identifier, parsed::Identifier),
    InappropriateTuplePattern(Vec<Pattern>, Span, Type),
    RigidTypeMismatch(parsed::Identifier, parsed::Identifier, Span),
    ConstructorMismatch(
        parsed::Identifier,
        Vec<Type>,
        parsed::Identifier,
        Vec<Type>,
        Span,
    ),
    TypeMismatch(Type, Type),
    UndefinedEnumVariant(Enum, parsed::Identifier),
}

pub type Result<T> = result::Result<T, Error>;
