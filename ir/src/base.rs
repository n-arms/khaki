use crate::parsed::comma_list;
pub use crate::parsed::Identifier;
use std::fmt;

#[derive(Clone)]
pub struct Program {
    pub definitions: Vec<Definition>,
    pub functions: Vec<Function>,
}

#[derive(Clone)]
pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<Variable>,
    pub body: Block,
}

#[derive(Clone)]
pub enum Definition {
    Struct(Struct),
    Enum(Enum),
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
    pub result: Variable,
}

#[derive(Clone)]
pub struct Stmt {
    pub var: Variable,
    pub value: Expr,
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
    Match {
        head: Variable,
        cases: Vec<MatchCase>,
    },
}

#[derive(Clone)]
pub struct MatchCase {
    pub variant: Identifier,
    pub binding: Variable,
    pub body: Block,
}

impl Variable {
    pub fn new(name: Identifier, typ: Type) -> Self {
        Self { name, typ }
    }
}

impl Definition {
    pub fn name(&self) -> &Identifier {
        match self {
            Definition::Struct(def) => &def.name,
            Definition::Enum(def) => &def.name,
        }
    }
}

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for def in &self.definitions {
            def.fmt(f)?;
        }
        for func in &self.functions {
            func.fmt(f)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Definition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Definition::Struct(def) => def.fmt(f),
            Definition::Enum(def) => def.fmt(f),
        }
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt(0, f)
    }
}

impl Expr {
    pub fn fmt(&self, ind: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Integer(int) => write!(f, "{int}"),
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
            Expr::Match { head, cases } => {
                writeln!(f, "match {} {{", head)?;
                for case in cases {
                    indent(ind + 1, f)?;
                    write!(f, "{:?}({}) => ", case.variant, case.binding)?;
                    case.body.fmt(ind + 2, f)?;
                    writeln!(f)?;
                }
                indent(ind, f)?;
                write!(f, "}}")
            }
        }
    }
}

impl Block {
    pub fn fmt(&self, ind: usize, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{{\n")?;

        for stmt in &self.stmts {
            stmt.fmt(ind + 1, f)?;
        }
        indent(ind + 1, f)?;
        writeln!(f, "return {};", self.result)?;
        indent(ind, f)?;
        write!(f, "}}")
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {:?}(", self.name)?;
        comma_list(f, &self.arguments)?;
        writeln!(f, ") -> {:?} ", self.body.result.typ)?;
        self.body.fmt(0, f)?;
        writeln!(f)
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
        write!(f, "let {:?} = ", self.var)?;
        self.value.fmt(ind + 1, f)?;
        writeln!(f, ";")
    }
}

fn indent(ind: usize, f: &mut fmt::Formatter) -> fmt::Result {
    for _ in 0..ind {
        write!(f, "  ")?;
    }
    Ok(())
}
