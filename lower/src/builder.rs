use std::collections::HashMap;

use ir::{
    base::{Block, Definition, Enum, EnumCase, Stmt, Struct, StructField, Type, Variable},
    hir::Identifier,
};

#[derive(Default)]
pub(crate) struct Builder {
    stmts: Vec<Stmt>,
}

impl Builder {
    pub(crate) fn stmt(&mut self, stmt: Stmt) -> &mut Self {
        self.stmts.push(stmt);
        self
    }

    pub(crate) fn build(self, result: Variable) -> Block {
        Block {
            stmts: self.stmts,
            result,
        }
    }
}

#[derive(Default)]
pub(crate) struct Env {
    definitions: Vec<Definition>,
    names: usize,
    tuples: HashMap<Vec<Type>, Identifier>,
}

impl Env {
    pub(crate) fn enum_def(&mut self, name: Identifier, cases: Vec<EnumCase>) {
        self.definitions
            .push(Definition::Enum(Enum { name, cases }))
    }

    pub(crate) fn tuple_name(&mut self, fields: &[Type]) -> Identifier {
        if let Some(name) = self.tuples.get(fields) {
            name.clone()
        } else {
            let name = self.fresh_name("tuple");
            self.tuples.insert(fields.to_vec(), name.clone());
            self.definitions.push(Definition::Struct(Struct {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|typ| StructField::new(typ.clone()))
                    .collect(),
            }));
            name
        }
    }

    pub(crate) fn fresh_name(&mut self, prefix: &str) -> Identifier {
        let token = self.names;
        self.names += 1;
        Identifier::from(format!("{prefix}_{token}"))
    }

    pub(crate) fn fresh_var(&mut self, typ: Type) -> Variable {
        Variable {
            name: self.fresh_name("var"),
            typ,
        }
    }

    pub(crate) fn definitions(self) -> Vec<Definition> {
        self.definitions
    }
}
