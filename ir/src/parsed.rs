use std::{
    borrow::Borrow,
    cell::{Ref, RefCell},
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
    pub set: LambdaSet,
    pub span: Span,
}

#[derive(Clone)]
pub struct Identifier {
    pub name: hir::Identifier,
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
    pub generics: Vec<Identifier>,
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
    Variable(Identifier, Vec<Type>, Option<Type>),
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
        generics: Vec<Type>,
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
    pub typ: Option<Type>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone)]
pub enum Pattern {
    Variable(Identifier, Option<Type>),
    Tuple(Vec<Pattern>, Span),
}
#[derive(Clone)]
pub enum Type {
    Integer(Span),
    Unification(VariableCell),
    Rigid(Identifier),
    Function(Vec<Type>, Box<Type>, LambdaSet, Span),
    Tuple(Vec<Type>, Span),
    Constructor(Identifier, Vec<Type>, Span),
}

#[derive(Clone)]
pub struct VariableCell {
    inner: Rc<RefCell<RawVariableCell>>,
}

enum RawVariableCell {
    Variable(Identifier),
    Type(Type),
}

impl Function {
    pub fn typ(&self) -> Type {
        Type::Function(
            self.arguments.iter().map(|arg| arg.typ.clone()).collect(),
            Box::new(self.result.clone()),
            self.set.clone(),
            self.span.clone(),
        )
    }
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
    pub fn dummy() -> Self {
        Self { start: 0, end: 0 }
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
            Pattern::Variable(name, _) => name.span,
            Pattern::Tuple(_, span) => *span,
        }
    }

    pub fn typ(&self) -> Type {
        match self {
            Pattern::Variable(_, typ) => typ.clone().unwrap(),
            Pattern::Tuple(patterns, span) => {
                Type::Tuple(patterns.iter().map(Pattern::typ).collect(), span.clone())
            }
        }
    }
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Integer(span) => *span,
            Type::Unification(name) => name.span(),
            Type::Rigid(name) => name.span,
            Type::Function(_, _, _, span) => *span,
            Type::Tuple(_, span) => *span,
            Type::Constructor(_, _, span) => *span,
        }
    }

    pub fn normalize(&self) -> Self {
        match self {
            Type::Integer(_) | Type::Rigid(_) => self.clone(),
            Type::Unification(cell) => {
                if cell.is_some() {
                    cell.get().clone()
                } else {
                    self.clone()
                }
            }
            Type::Function(args, res, set, span) => Type::Function(
                args.iter().map(Type::normalize).collect(),
                Box::new(res.normalize()),
                set.clone(),
                *span,
            ),
            Type::Tuple(elems, span) => {
                Type::Tuple(elems.iter().map(Type::normalize).collect(), *span)
            }
            Type::Constructor(name, args, span) => Type::Constructor(
                name.clone(),
                args.iter().map(Type::normalize).collect(),
                *span,
            ),
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

    pub fn is_some(&self) -> bool {
        matches!(*self.inner.as_ref().borrow(), RawVariableCell::Type(_))
    }

    pub fn get(&self) -> Ref<Type> {
        Ref::map(self.inner.as_ref().borrow(), |raw| {
            if let RawVariableCell::Type(typ) = raw {
                typ
            } else {
                unreachable!()
            }
        })
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
            Type::Unification(name) => match name.inner.as_ref().borrow().deref() {
                RawVariableCell::Variable(name) => name.fmt(f),
                RawVariableCell::Type(typ) => typ.fmt(f),
            },
            Type::Rigid(name) => write!(f, "{:?}", name),
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
            Type::Constructor(name, args, _) => {
                name.fmt(f)?;
                write!(f, "[")?;
                comma_list(f, args)?;
                write!(f, "]")
            }
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
            Pattern::Variable(name, _) => name.fmt(f),
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
            Expr::Variable(name, _, _) => write!(f, "{:?}", name),
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
            Expr::Variable(name, _, _) => name.span,
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
        write!(f, "enum {:?}[", self.name)?;
        comma_list(f, &self.generics)?;
        write!(f, "] {{")?;
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

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..={}", self.start, self.end)
    }
}
