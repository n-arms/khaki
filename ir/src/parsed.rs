use core::fmt;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    pub name: String,
}

impl<'a> From<&'a str> for Identifier {
    fn from(value: &'a str) -> Self {
        Self { name: value.into() }
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
        generics: Vec<Type>,
        arguments: Vec<Expr>,
    },
    Function {
        captures: Vec<Argument>,
        arguments: Vec<Argument>,
        result: Type,
        body: Box<Expr>,
    },
}

#[derive(Clone)]
pub enum Type {
    Integer,
    Variable(Identifier),
    Function(Vec<Type>, Box<Type>),
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
            Type::Function(args, result) => {
                if args.len() == 1 {
                    write!(f, "{:?}", args[0])?;
                } else {
                    write!(f, "(")?;
                    comma_list(f, args)?;
                    write!(f, ")")?;
                }
                write!(f, " -> {:?}", result)
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
                body,
            } => {
                write!(f, "[")?;
                comma_list(f, captures)?;
                write!(f, "](")?;
                comma_list(f, arguments)?;
                write!(f, ") -> {:?} = {:?}", result, body)
            }
        }
    }
}
