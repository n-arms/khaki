use core::fmt;
use std::{cell::RefCell, rc::Rc};

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
            LambdaSet::one(self.name.clone(), token),
        )
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
        typ: Option<Type>,
    },
    FunctionCall {
        function: Identifier,
        set: LambdaSet,
        generics: Vec<Type>,
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
}

#[derive(Clone)]
pub enum Type {
    Integer,
    Variable(Identifier),
    Function(Vec<Type>, Box<Type>, LambdaSet),
}

#[derive(Clone)]
pub struct LambdaSet {
    pub pool: Rc<RefCell<Vec<Identifier>>>,
    pub token: usize,
}
impl LambdaSet {
    pub fn one(lambda: Identifier, token: usize) -> LambdaSet {
        LambdaSet {
            pool: Rc::new(RefCell::new(vec![lambda.clone()])),
            token,
        }
    }

    pub fn dummy() -> LambdaSet {
        LambdaSet {
            pool: Rc::default(),
            token: 0,
        }
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

fn comma_list<I: fmt::Debug>(
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
                write!(f, " - {:?} -> {:?}", set.pool.borrow(), result)
            }
            Type::Variable(name) => write!(f, "{:?}", name),
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(int) => write!(f, "{}", int),
            Expr::Variable { name, typ } => write!(f, "{:?}", name),
            Expr::FunctionCall {
                function,
                generics,
                arguments,
                set,
            } => {
                write!(f, "{:?}", function)?;
                if !generics.is_empty() {
                    write!(f, "[")?;
                    comma_list(f, generics)?;
                    write!(f, "]")?;
                }
                write!(f, "(")?;
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
                write!(
                    f,
                    ") - {:?} -> {:?} = {:?}",
                    set.pool.borrow(),
                    result,
                    body
                )
            }
            Expr::Tuple(elems) => {
                write!(f, "<|")?;
                comma_list(f, elems)?;
                write!(f, "|>")
            }
        }
    }
}
