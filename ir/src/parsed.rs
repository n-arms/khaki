use std::{
    borrow::Borrow,
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fmt, hash,
    ops::{Deref, RangeInclusive},
    rc::Rc,
};

pub use crate::hir::{self, LambdaSet};
use crate::{base::indent, hir::comma_list};

#[derive(Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub enums: HashMap<Identifier, Enum>,
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
    pub span: Span,
}

#[derive(Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone)]
pub struct Enum {
    pub name: Identifier,
    pub cases: Vec<EnumCase>,
    pub span: Span,
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
}

#[derive(Clone)]
pub enum Expr {
    Integer(i32, Span),
    Variable(Identifier),
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
        name: Identifier,
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
    pub span: Span,
}

#[derive(Clone)]
pub enum Pattern {
    Variable(Identifier),
    Tuple(Vec<Pattern>, Span),
}

#[derive(Clone)]
pub enum Type {
    Integer(Span),
    Variable(VariableCell),
    Function(Vec<Type>, Box<Type>, LambdaSet, Span),
    Tuple(Vec<Type>, Span),
    Constructor(Identifier),
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
    pub fn new(name: String, span: Span) -> Self {
        Self {
            name: hir::Identifier::from(name),
            span,
        }
    }
}

impl Span {
    /*
    pub fn dummy() -> Self {
        todo!()
    }

    pub fn range(self) -> RangeInclusive<usize> {
        self.start..=self.end
    }

    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Variable(name) => name.span,
            Pattern::Tuple(_, span) => *span,
        }
    }
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Integer(span) => *span,
            Type::Variable(name) => name.span(),
            Type::Function(_, _, _, span) => *span,
            Type::Tuple(_, span) => *span,
            Type::Constructor(name) => name.span,
        }
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

    pub fn span(&self) -> Span {
        match self.inner.as_ref().borrow().deref() {
            RawVariableCell::Variable(name) => name.span,
            RawVariableCell::Type(typ) => typ.span(),
        }
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
            Type::Variable(name) => match name.inner.as_ref().borrow().deref() {
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
            Type::Constructor(name) => name.fmt(f),
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
            Expr::Variable(name) => write!(f, "{:?}", name),
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

    pub fn span(&self) -> Span {
        match self {
            Expr::Integer(_, span) => *span,
            Expr::Variable(name) => name.span,
            Expr::FunctionCall { span, .. } => *span,
            Expr::Function { span, .. } => *span,
            Expr::Tuple(_, span) => *span,
            Expr::TupleAccess(_, _, span) => *span,
            Expr::Enum { span, .. } => *span,
            Expr::Match { span, .. } => *span,
        }
    }
}

impl hash::Hash for Identifier {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name)
    }
}

impl Eq for Identifier {}

impl PartialOrd for Identifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Identifier {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
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

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for def in self.enums.values() {
            writeln!(f, "{:?}", def)?;
        }
        for func in &self.functions {
            writeln!(f, "{:?}", func)?;
        }
        Ok(())
    }
}
