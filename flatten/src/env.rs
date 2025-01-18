use std::collections::HashMap;

use ir::hir::{Argument, Enum, EnumCase, Expr, Function, Identifier, Program, Type};

pub(crate) struct Env {
    generated: HashMap<Signature, Identifier>,
    names: usize,
    functions: Vec<Function>,
    enums: Vec<Enum>,
    old_functions: HashMap<Identifier, Function>,
    old_enums: HashMap<Identifier, Enum>,
}

impl Env {
    pub(crate) fn new(program: &Program) -> Self {
        Self {
            generated: HashMap::new(),
            names: 0,
            functions: Vec::new(),
            enums: Vec::new(),
            old_functions: program
                .functions
                .iter()
                .map(|func| (func.name.clone(), func.clone()))
                .collect(),
            old_enums: program.enums.clone(),
        }
    }
    pub(crate) fn generate(&mut self, name: Identifier, generics: Vec<Type>, new_name: Identifier) {
        self.generated
            .insert(Signature { name, generics }, new_name);
    }

    pub(crate) fn generated(
        &mut self,
        name: Identifier,
        generics: Vec<Type>,
    ) -> Option<Identifier> {
        self.generated.get(&Signature { name, generics }).cloned()
    }

    pub(crate) fn fresh(&mut self, prefix: &str) -> Identifier {
        let token = self.names;
        self.names += 1;
        Identifier::from(format!("{}_{}", prefix, token))
    }

    pub(crate) fn function(
        &mut self,
        name: Identifier,
        arguments: Vec<Argument>,
        result: Type,
        body: Expr,
    ) {
        println!("function {:?}", name);
        self.functions.push(Function {
            name,
            arguments,
            generics: Vec::new(),
            result,
            body,
        });
    }

    pub(crate) fn def(&mut self, name: Identifier, cases: Vec<EnumCase>) {
        println!("def {:?}", name);
        self.enums.push(Enum {
            name,
            generics: Vec::new(),
            cases,
        });
    }

    pub(crate) fn build(self) -> Program {
        let enums = self
            .enums
            .into_iter()
            .map(|def| (def.name.clone(), def))
            .collect();
        Program {
            functions: self.functions,
            enums,
        }
    }

    pub(crate) fn get_enum(&self, name: &Identifier) -> &Enum {
        &self.old_enums[name]
    }

    pub(crate) fn get_function(&self, name: &Identifier) -> &Function {
        &self.old_functions[name]
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Signature {
    name: Identifier,
    generics: Vec<Type>,
}
