use std::{cell::RefCell, fmt, ops::Deref, rc::Rc};

pub use crate::hir::{self, LambdaSet};
use crate::{base::indent, hir::comma_list};

#[derive(Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub enums: Vec<Enum>,
}

#[derive(Clone)]
pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<Argument>,
    pub generics: Vec<Identifier>,
    pub result: Type,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone)]
pub struct Identifier {
    name: hir::Identifier,
    span: Span,
}

#[derive(Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub text: String,
}

#[derive(Clone)]
pub struct Enum {
    pub name: Identifier,
    pub cases: Vec<EnumCase>,
}

#[derive(Clone)]
pub struct EnumCase {
    pub tag: Identifier,
    pub typ: Type,
    pub span: Span,
}

#[derive(Clone)]
pub struct Argument {
    pub binding: Pattern,
    pub typ: Type,
    pub span: Span,
}

#[derive(Clone)]
pub enum Expr {
    Integer(i32, Span),
    Variable(Identifier, Span),
    FunctionCall {
        function: Box<Expr>,
        set: LambdaSet,
        arguments: Vec<Expr>,
        span: Span,
    },
    Function {
        captures: Vec<Identifier>,
        arguments: Vec<ClosureArgument>,
        result: Option<Type>,
        body: Box<Expr>,
        set: LambdaSet,
        name: hir::Identifier,
        span: Span,
    },
    Tuple(Vec<Expr>, Span),
    TupleAccess(Box<Expr>, usize, Span),
    Enum {
        typ: Identifier,
        tag: Identifier,
        argument: Box<Expr>,
        span: Span,
    },
    Match {
        head: Box<Expr>,
        cases: Vec<MatchCase>,
        span: Span,
    },
}

#[derive(Clone)]
pub struct ClosureArgument {
    pub binding: Pattern,
    pub typ: Option<Type>,
    pub span: Span,
}

#[derive(Clone)]
pub struct MatchCase {
    pub variant: Identifier,
    pub binding: Pattern,
    pub body: Expr,
}

#[derive(Clone)]
pub enum Pattern {
    Variable(Identifier),
    Tuple(Vec<Pattern>, Span),
}

#[derive(Clone)]
pub enum Type {
    Integer(Span),
    Variable(VariableCell, Span),
    Function(Vec<Type>, Box<Type>, LambdaSet, Span),
    Tuple(Vec<Type>, Span),
    Constructor(Identifier, Span),
}

#[derive(Clone)]
pub struct VariableCell {
    inner: Rc<RefCell<RawVariableCell>>,
}

enum RawVariableCell {
    Variable(Identifier),
    Type(Type),
}

impl Identifier {
    pub fn new(name: hir::Identifier, span: Span) -> Self {
        Self { name, span }
    }
}

impl Span {
    pub fn dummy() -> Self {
        todo!()
    }
}

impl VariableCell {
    pub fn new(name: Identifier) -> Self {
        Self {
            inner: Rc::new(RefCell::new(RawVariableCell::Variable(name))),
        }
    }

    pub fn set(&self, typ: Type) {
        *self.inner.as_ref().borrow_mut() = RawVariableCell::Type(typ)
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name.name)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer(_) => write!(f, "Int"),
            Type::Variable(name, _) => match name.inner.as_ref().borrow().deref() {
                RawVariableCell::Variable(name) => name.fmt(f),
                RawVariableCell::Type(typ) => typ.fmt(f),
            },
            Type::Function(args, result, set, _) => {
                write!(f, "(")?;
                comma_list(f, args)?;
                write!(f, ") -{:?}-> {:?}", set.token, result)
            }
            Type::Tuple(elems, _) => {
                write!(f, "(")?;
                comma_list(f, elems)?;
                write!(f, ")")
            }
            Type::Constructor(name, _) => name.fmt(f),
        }
    }
}

impl fmt::Debug for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.binding, self.typ)
    }
}

impl fmt::Debug for ClosureArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(typ) = &self.typ {
            write!(f, "{:?}: {:?}", self.binding, typ)
        } else {
            write!(f, "{:?}", self.binding)
        }
    }
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Variable(name) => name.fmt(f),
            Pattern::Tuple(elems, _) => {
                write!(f, "(")?;
                comma_list(f, elems)?;
                write!(f, ")")
            }
        }
    }
}

impl Expr {
    pub fn fmt(&self, ind: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Integer(int, _) => write!(f, "{int}"),
            Expr::Variable(name, _) => write!(f, "{:?}", name),
            Expr::FunctionCall {
                function,
                set,
                arguments,
                ..
            } => {
                if let Expr::Variable(..) = function.as_ref() {
                    function.fmt(ind, f)?;
                } else {
                    write!(f, "(")?;
                    function.fmt(ind, f)?;
                    write!(f, "):{:?}(", set.token)?;
                }

                let mut first = true;
                for arg in arguments {
                    if first {
                        first = false;
                    } else {
                        write!(f, ",")?;
                    }
                    arg.fmt(ind, f)?;
                }
                write!(f, ")")
            }
            Expr::Function {
                captures,
                arguments,
                result,
                body,
                set,
                ..
            } => {
                write!(f, "[")?;
                comma_list(f, captures)?;
                write!(f, "](")?;
                comma_list(f, arguments)?;
                write!(f, ") -{:?}", set.token)?;
                if let Some(typ) = &result {
                    write!(f, "-> {:?}", typ)?;
                }
                write!(f, " = ")?;
                body.fmt(ind, f)
            }
            Expr::Tuple(elems, _) => {
                write!(f, "<|")?;
                let mut first = true;
                for elem in elems {
                    if first {
                        first = false;
                    } else {
                        write!(f, ",")?;
                    }
                    elem.fmt(ind, f)?;
                }
                write!(f, "|>")
            }
            Expr::TupleAccess(tuple, field, _) => {
                tuple.fmt(ind, f)?;
                write!(f, ".{:?}", field)
            }
            Expr::Enum {
                typ, tag, argument, ..
            } => {
                write!(f, "{:?}::{:?}(", typ, tag)?;
                argument.fmt(ind, f)?;
                write!(f, ")")
            }
            Expr::Match { head, cases, .. } => {
                write!(f, "match ")?;
                head.fmt(ind, f)?;
                writeln!(f, " {{")?;

                let mut first = true;
                for case in cases {
                    if first {
                        first = false;
                    } else {
                        writeln!(f, ",")?;
                    }
                    indent(ind + 1, f)?;
                    write!(f, "{:?}({:?}) => ", case.variant, case.binding)?;
                    case.body.fmt(ind + 1, f)?;
                }
                write!(f, "\n}}")
            }
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {:?}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "[")?;
            comma_list(f, &self.generics)?;
            write!(f, "]")?;
        }
        write!(f, "(")?;
        comma_list(f, &self.arguments)?;
        write!(f, ") -> {:?} = ", self.result)?;
        self.body.fmt(0, f)
    }
}

impl fmt::Debug for Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "enum {:?} {{", self.name)?;
        comma_list(f, &self.cases)?;
        write!(f, "}}")
    }
}

impl fmt::Debug for EnumCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}({:?})", self.tag, self.typ)
    }
}
