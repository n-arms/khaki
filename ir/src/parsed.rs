use core::fmt;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    pub name: String,
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

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {:?}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "[")?;
            let mut first = true;
            for generic in &self.generics {
                if first {
                    write!(f, "{:?}", generic)?;
                    first = false;
                } else {
                    write!(f, ", {:?}", generic)?;
                }
            }
            write!(f, "]")?;
        }
        write!(f, "(")?;
        let mut first = true;
        for Argument { name, typ } in &self.arguments {
            if first {
                write!(f, "{:?}: {:?}", name, typ)?;
                first = false;
            } else {
                write!(f, ", {:?}: {:?}", name, typ)?;
            }
        }
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
                    let mut first = true;
                    for arg in args {
                        if first {
                            first = false;
                            write!(f, "{:?}", arg)?;
                        } else {
                            write!(f, ", {:?}", arg)?;
                        }
                    }
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
                    let mut first = true;
                    for generic in generics {
                        if first {
                            first = false;
                            write!(f, "{:?}", generic)?;
                        } else {
                            write!(f, ", {:?}", generic)?;
                        }
                    }
                    write!(f, "]")?;
                }
                write!(f, "(")?;
                let mut first = true;
                for arg in arguments {
                    if first {
                        first = false;
                        write!(f, "{:?}", arg)?;
                    } else {
                        write!(f, ", {:?}", arg)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}
