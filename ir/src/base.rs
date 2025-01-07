use crate::parsed::comma_list;
pub use crate::parsed::Identifier;
use std::fmt;

#[derive(Clone)]
pub struct Program {
    pub enums: Vec<Enum>,
    pub structs: Vec<Struct>,
    pub functions: Vec<Function>,
}

#[derive(Clone)]
pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<Variable>,
    pub body: Vec<Stmt>,
    pub result: Variable,
}

#[derive(Clone)]
pub struct Struct {
    pub name: Identifier,
    pub fields: Vec<Type>,
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
pub struct Variable {
    pub name: Identifier,
    pub typ: Type,
}

#[derive(Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone)]
pub enum Stmt {
    Let {
        var: Variable,
        value: Expr,
    },
    Match {
        head: Variable,
        cases: Vec<MatchCase>,
    },
}

#[derive(Clone)]
pub enum Expr {
    Integer(i32),
    Variable(Variable),
    DirectCall {
        function: Identifier,
        arguments: Vec<Variable>,
    },
    Tuple(Vec<Variable>),
    TupleAccess(Variable, usize),
    Enum {
        typ: Identifier,
        tag: Identifier,
        argument: Variable,
    },
}

#[derive(Clone)]
pub struct MatchCase {
    pub variant: Identifier,
    pub binding: Variable,
    pub body: Vec<Stmt>,
}

impl Variable {
    pub fn new(name: Identifier, typ: Type) -> Self {
        Self { name, typ }
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for enum_def in &self.enums {
            enum_def.fmt(f)?;
        }
        for struct_def in &self.structs {
            struct_def.fmt(f)?;
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

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.name, self.typ)
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.name.fmt(f)
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(int) => int.fmt(f),
            Expr::Variable(name) => write!(f, "{name}"),
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
                write!(f, "{:?}::{:?}({})", typ, tag, argument)
            }
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {:?}(", self.name)?;
        comma_list(f, &self.arguments)?;
        writeln!(f, ") -> {:?} {{", self.result.typ)?;
        for stmt in &self.body {
            stmt.fmt(1, f)?;
        }
        indent(1, f)?;
        writeln!(f, "return {};\n}}", self.result)
    }
}

impl fmt::Debug for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "struct {:?} {{", self.name)?;
        comma_list(f, &self.fields)?;
        writeln!(f, "}}")
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
        writeln!(f, "}}")
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
            Stmt::Let { var, value } => {
                writeln!(f, "let {:?} = {:?};", var, value)
            }
            Stmt::Match { head, cases } => {
                writeln!(f, "match {} {{", head)?;
                for case in cases {
                    indent(ind + 1, f)?;
                    writeln!(f, "{:?}({}) => {{", case.variant, case.binding)?;
                    for stmt in &case.body {
                        stmt.fmt(ind + 2, f)?;
                    }
                    indent(ind + 1, f)?;
                    writeln!(f, "}}")?;
                }
                writeln!(f, "}}")
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
