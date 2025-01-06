use crate::parsed::comma_list;
pub use crate::parsed::Identifier;
use std::fmt;

#[derive(Clone)]
pub struct Program {
    pub enums: Vec<Enum>,
    pub functions: Vec<Function>,
}

#[derive(Clone)]
pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<Argument>,
    pub typ: Type,
    pub body: Vec<Stmt>,
    pub result: Identifier,
}

#[derive(Clone)]
pub struct Enum {
    pub name: Identifier,
    pub cases: Vec<(Identifier, Type)>,
}

#[derive(Clone)]
pub enum Type {
    Constructor(Identifier),
    Integer,
}

#[derive(Clone)]
pub struct Argument {
    pub name: Identifier,
    pub typ: Type,
}

#[derive(Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub result: Identifier,
    pub typ: Identifier,
}

#[derive(Clone)]
pub enum Stmt {
    Let {
        name: Identifier,
        typ: Type,
        value: Expr,
    },
    Match {
        head: Identifier,
        cases: Vec<MatchCase>,
    },
}

#[derive(Clone)]
pub enum Expr {
    Integer(i32),
    Variable(Identifier),
    DirectCall {
        function: Identifier,
        arguments: Vec<Identifier>,
    },
    Tuple(Vec<Identifier>),
    TupleAccess(Identifier, usize),
    Enum {
        typ: Identifier,
        tag: Identifier,
        argument: Identifier,
    },
}

#[derive(Clone)]
pub struct MatchCase {
    pub variant: Identifier,
    pub binding: Identifier,
    pub body: Vec<Stmt>,
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for enum_def in &self.enums {
            enum_def.fmt(f)?;
        }
        for func in &self.functions {
            func.fmt(f)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Constructor(name) => write!(f, "{:?}", name),
            Type::Integer => write!(f, "Int"),
        }
    }
}

impl fmt::Debug for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.name, self.typ)
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(int) => int.fmt(f),
            Expr::Variable(name) => name.fmt(f),
            Expr::DirectCall {
                function,
                arguments,
            } => {
                write!(f, "{:?}(", function)?;
                comma_list(f, arguments)?;
                write!(f, ")")
            }
            Expr::Tuple(elems) => {
                write!(f, "(")?;
                comma_list(f, elems)?;
                write!(f, ")")
            }
            Expr::TupleAccess(tuple, field) => {
                write!(f, "{:?}.{}", tuple, field)
            }
            Expr::Enum { typ, tag, argument } => {
                write!(f, "{:?}::{:?}({:?})", typ, tag, argument)
            }
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {:?}(", self.name)?;
        comma_list(f, &self.arguments)?;
        write!(f, ") -> {:?} {{\n", self.typ)?;
        for stmt in &self.body {
            stmt.fmt(1, f)?;
        }
        indent(1, f)?;
        write!(f, "return {:?};\n}}\n", self.result)
    }
}

impl fmt::Debug for Enum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "enum {:?} {{", self.name)?;
        comma_list(
            f,
            self.cases
                .iter()
                .map(|(name, typ)| format!("{:?}({:?})", name, typ)),
        )?;
        write!(f, "}}\n")
    }
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(0, f)
    }
}

impl Stmt {
    pub fn fmt(&self, ind: usize, f: &mut fmt::Formatter) -> fmt::Result {
        indent(ind, f)?;
        match self {
            Stmt::Let { name, typ, value } => {
                write!(f, "let {:?}: {:?} = {:?};\n", name, typ, value)
            }
            Stmt::Match { head, cases } => {
                write!(f, "match {:?} {{\n", head)?;
                for case in cases {
                    indent(ind + 1, f)?;
                    write!(f, "{:?}({:?}) => {{\n", case.variant, case.binding)?;
                    for stmt in &case.body {
                        stmt.fmt(ind + 2, f)?;
                    }
                    indent(ind + 1, f)?;
                    write!(f, "}}\n")?;
                }
                write!(f, "}}\n")
            }
        }
    }
}

fn indent(ind: usize, f: &mut fmt::Formatter) -> fmt::Result {
    for _ in 0..ind {
        write!(f, "  ")?;
    }
    Ok(())
}
