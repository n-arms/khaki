macro_rules! generate {
    ($gen:expr, $format:literal $(, $arg:expr)*) => {
        $gen.push_str(&format!($format $(, $arg)*));
    };
}

use std::collections::HashMap;

pub(crate) use generate;
use ir::parsed::{Identifier, Type};

use crate::typ_to_string;

#[derive(Default)]
pub struct Generator {
    text: String,
    line: String,
    indent: usize,
}

impl Generator {
    pub fn push(&mut self, char: char) {
        self.line.push(char);
    }

    pub fn push_str(&mut self, str: &str) {
        self.line.push_str(str);
    }

    pub fn newline(&mut self) {
        for _ in 0..self.indent {
            self.text.push(' ');
        }
        self.text.push_str(&self.line);
        self.text.push('\n');
        self.line.clear();
    }

    pub fn inc(&mut self) {
        self.indent += 1;
    }

    pub fn dec(&mut self) {
        self.indent -= 1;
    }

    pub fn scope<T>(&mut self, inner: impl FnOnce(&mut Self) -> T) -> T {
        self.inc();
        let result = inner(self);
        self.dec();
        result
    }

    pub fn comma_list<T>(
        &mut self,
        list: impl IntoIterator<Item = T>,
        mut func: impl FnMut(&mut Self, T),
    ) {
        self.scope(|mut gen| {
            let mut first = true;
            for elem in list {
                if first {
                    first = false;
                } else {
                    gen.push_str(", ");
                }
                func(&mut gen, elem);
            }
        });
    }

    pub fn generate(mut self) -> String {
        if !self.line.is_empty() {
            self.newline();
        }
        self.text
    }
}

#[derive(Default)]
pub struct Env {
    pub preamble: Generator,
    pub main: Generator,
    names: usize,
    tuples: HashMap<Vec<Type>, Identifier>,
}

impl Env {
    pub fn fresh_name(&mut self, prefix: &str) -> String {
        let name = self.names;
        self.names += 1;
        format!("{prefix}_{name}")
    }

    pub fn generate(self) -> String {
        let mut text = String::from("//=== preamble ===\n");
        text.push_str(&self.preamble.generate());
        text.push_str("//=== main ===\n");
        text.push_str(&self.main.generate());
        text
    }

    pub fn tuple_name(&mut self, tuple: &[Type]) -> Identifier {
        if let Some(id) = self.tuples.get(tuple) {
            id.clone()
        } else {
            let name = Identifier::from(self.fresh_name("tuple"));
            self.tuples.insert(tuple.to_vec(), name.clone());

            generate_tuple_struct(tuple, name.clone(), self);

            name
        }
    }
}

fn generate_tuple_struct(tuple: &[Type], name: Identifier, env: &mut Env) {
    generate!(&mut env.preamble, "struct {} {{", name.name);
    env.preamble.newline();
    env.preamble.inc();
    for (i, typ) in tuple.iter().enumerate() {
        let typ_name = typ_to_string(typ, env);
        generate!(&mut env.preamble, "{} field{};", typ_name, i);
        env.preamble.newline();
    }
    env.preamble.dec();
    generate!(&mut env.preamble, "}};");
}
