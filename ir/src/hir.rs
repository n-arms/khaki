use core::fmt;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    pub name: String,
}

impl Identifier {
    pub fn dummy() -> Self {
        Self {
            name: String::from("dummy"),
        }
    }
}

impl<'a> From<&'a str> for Identifier {
    fn from(value: &'a str) -> Self {
        Self { name: value.into() }
    }
}

impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Self { name }
    }
}

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
}

impl Function {
    pub fn typ(&self, token: usize) -> Type {
        Type::Function(
            self.arguments.iter().map(|arg| arg.typ.clone()).collect(),
            Box::new(self.result.clone()),
            LambdaSet::new(token),
        )
    }
}

#[derive(Clone)]
pub struct Enum {
    pub name: Identifier,
    pub cases: Vec<(Identifier, Type)>,
}

impl Enum {
    pub fn variant_type(&self, variant: &Identifier) -> &Type {
        for (name, typ) in &self.cases {
            if name == variant {
                return typ;
            }
        }
        unreachable!()
    }
}

#[derive(Clone)]
pub struct Argument {
    pub name: Identifier,
    pub typ: Type,
}

#[derive(Clone)]
pub enum Expr {
    Integer(i32),
    Variable {
        name: Identifier,
        generics: Vec<Type>,
        typ: Type,
    },
    FunctionCall {
        function: Box<Expr>,
        set: LambdaSet,
        arguments: Vec<Expr>,
    },
    Function {
        captures: Vec<Argument>,
        arguments: Vec<Argument>,
        result: Type,
        body: Box<Expr>,
        set: LambdaSet,
        name: Identifier,
    },
    Tuple(Vec<Expr>),
    TupleAccess(Box<Expr>, usize),
    Enum {
        typ: Identifier,
        tag: Identifier,
        generics: Vec<Type>,
        argument: Box<Expr>,
    },
    Match {
        head: Box<Expr>,
        cases: Vec<MatchCase>,
    },
}

#[derive(Clone)]
pub struct MatchCase {
    pub variant: Identifier,
    pub binding: Identifier,
    pub binding_type: Type,
    pub body: Expr,
}

impl Expr {
    pub fn typ(&self) -> Type {
        match self {
            Expr::Integer(_) => Type::Integer,
            Expr::Variable { typ, .. } => typ.clone(),
            Expr::FunctionCall { function, .. } => {
                if let Type::Function(_, res, _) = function.typ() {
                    *res
                } else {
                    unreachable!()
                }
            }
            Expr::Function {
                arguments,
                result,
                set,
                ..
            } => Type::Function(
                arguments.iter().map(|arg| arg.typ.clone()).collect(),
                Box::new(result.clone()),
                set.clone(),
            ),
            Expr::Tuple(elems) => Type::Tuple(elems.iter().map(|e| e.typ()).collect()),
            Expr::TupleAccess(tuple, field) => {
                if let Type::Tuple(elems) = tuple.typ() {
                    elems[*field].clone()
                } else {
                    unreachable!()
                }
            }
            Expr::Enum { typ, .. } => Type::Constructor(typ.clone()),
            Expr::Match { cases, .. } => cases[0].body.typ(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Integer,
    Variable(Identifier),
    Function(Vec<Type>, Box<Type>, LambdaSet),
    Tuple(Vec<Type>),
    Constructor(Identifier),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LambdaSet {
    pub token: usize,
}

impl LambdaSet {
    pub fn dummy() -> Self {
        Self { token: 0 }
    }

    fn new(token: usize) -> Self {
        Self { token }
    }
}

impl Into<usize> for LambdaSet {
    fn into(self) -> usize {
        self.token
    }
}

impl From<usize> for LambdaSet {
    fn from(value: usize) -> Self {
        Self::new(value)
    }
}

/*
rules for lambda inference: every variable/expression that has a function type also has an associated lambda set.
initially each lambda set is just an unbound variable. in addition, we have accumulated some constraints, where each constraint
says that a certain variable has to contain a certain concrete lambda. in the end, we want a lambda to never change lambda sets as it
passes through a program. let's consider some case studies.

let x = []() -> Int = 3 // we infer to have set a
let y = []() -> Int = 4 // we infer to have set b

// we infer to have set e
let z = if _
    then x // we infer to have set c
    else y // we infer to have set d

we also collect the constraints lambda1 in a and lambda2 in b

the equality constraint between the types of x and y in the if statement leads to a *set equality* constraint between c and d.
since x and y remain fixed throughout the program, we also have equality constraints between a and c, and b and d.

after this series of inferences, we have all lambda sets pointing at the same memory pool. in order to resolve the two membership constraints,
we add lambda1 and lambda2


what about polymorphic lambdas? if a function returns a lambda we want to treat that function locally and not collect all instances of its usage before we construct a lambda set.
this means that we make functions that return lambdas accept a `set` generic argument that abstracts over the lambda set.

fun make_three() -> () -> Int = []() -> Int = 3

let x = make_three()
let y = []() -> Int = 4

let z = if _
    then x
    else y

we infer the set of `make_three` first:
fun make_three<a, lambda1 in a>() -> () -a-> Int = []() -> Int = 3

we can then infer the type of x:
let x = make_three[b]() // we infer to have set b, lambda1 in b
let y = []() -> Int = 4 // we infer to have set c, lambda2 in c

from which point we do the same unification as earlier. note that we *instantiate* `make_three` to have a concrete lambda set parameter and substitute out the constraints

what about about lambdas as arguments to functions? I think we can treat them the same way: collect constraints, then make a set variable with those constraints

there are therefore three forms of lambda set: a generic variable, a unification variable, a concrete set of lambdas

each labling of a lambda set should contain
*/

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for def in &self.enums {
            writeln!(f, "{:?}", def)?;
        }

        for func in &self.functions {
            writeln!(f, "{:?}", func)?;
        }

        Ok(())
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Debug for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.name, self.typ)
    }
}

pub(crate) fn comma_list<I: fmt::Debug>(
    f: &mut fmt::Formatter<'_>,
    list: impl IntoIterator<Item = I>,
) -> fmt::Result {
    let mut first = true;
    for elem in list {
        if first {
            first = false;
        } else {
            write!(f, ", ")?;
        }
        write!(f, "{:?}", elem)?;
    }
    Ok(())
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
        write!(f, ") -> {:?} = {:?}", self.result, self.body)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "Int"),
            Type::Function(args, result, set) => {
                if args.len() == 1 {
                    write!(f, "{:?}", args[0])?;
                } else {
                    write!(f, "(")?;
                    comma_list(f, args)?;
                    write!(f, ")")?;
                }
                write!(f, " - {:?} -> {:?}", set.token, result)
            }
            Type::Variable(name) => write!(f, "{:?}", name),
            Type::Tuple(elems) => {
                write!(f, "<|")?;
                comma_list(f, elems)?;
                write!(f, "|>")
            }
            Type::Constructor(name) => write!(f, "{:?}", name),
        }
    }
}

impl fmt::Debug for Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "enum {} {{", self.name.name)?;
        comma_list(
            f,
            self.cases
                .iter()
                .map(|(name, typ)| format!("{}({:?})", name.name, typ)),
        )?;
        writeln!(f, "}}")
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(int) => write!(f, "{}", int),
            Expr::Variable { name, generics, .. } => {
                write!(f, "{:?}", name)?;
                if generics.is_empty() {
                    Ok(())
                } else {
                    write!(f, "[")?;
                    comma_list(f, generics)?;
                    write!(f, "]")
                }
            }
            Expr::FunctionCall {
                function,
                arguments,
                set,
            } => {
                write!(f, "{:?}:{}(", function, set.token)?;
                comma_list(f, arguments)?;
                write!(f, ")")
            }
            Expr::Function {
                captures,
                arguments,
                result,
                set,
                body,
                ..
            } => {
                write!(f, "[")?;
                comma_list(f, captures)?;
                write!(f, "](")?;
                comma_list(f, arguments)?;
                write!(f, ") - {:?} -> {:?} = {:?}", set.token, result, body)
            }
            Expr::Tuple(elems) => {
                write!(f, "<|")?;
                comma_list(f, elems)?;
                write!(f, "|>")
            }
            Expr::TupleAccess(tuple, field) => write!(f, "{:?}.{}", tuple.as_ref(), field),
            Expr::Enum {
                typ,
                tag,
                generics,
                argument,
            } => {
                write!(f, "{:?}", typ)?;
                if !generics.is_empty() {
                    write!(f, "[")?;
                    comma_list(f, generics)?;
                    write!(f, "]")?;
                }
                write!(f, "::{:?}({:?})", tag, argument)
            }
            Expr::Match { head: expr, cases } => {
                write!(f, "match {:?} {{", expr)?;
                comma_list(f, cases)?;
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Debug for MatchCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}({:?}) => {:?}",
            self.variant, self.binding, self.body
        )
    }
}
