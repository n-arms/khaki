use std::collections::HashMap;

use ir::{
    base::{Definition, Enum, Struct, Type},
    hir::Identifier,
};

use crate::graph::{minimum_feedback_arc_set, Graph};

impl Graph for HashMap<Identifier, Definition> {
    type Node = Identifier;

    fn children<'a>(&'a self, node: &Self::Node) -> Vec<Self::Node> {
        constructors(&self[node])
    }

    fn nodes<'a>(&'a self) -> Vec<Self::Node> {
        self.values()
            .flat_map(|def| self.children(def.name()))
            .collect()
    }
}

fn constructors(def: &Definition) -> Vec<Identifier> {
    match def {
        Definition::Struct(Struct { fields, .. }) => fields
            .iter()
            .flat_map(|field| type_constructors(&field.typ))
            .collect(),
        Definition::Enum(Enum { cases, .. }) => cases
            .iter()
            .flat_map(|case| type_constructors(&case.typ))
            .collect(),
    }
}

fn type_constructors(typ: &Type) -> Vec<Identifier> {
    match typ {
        Type::Integer => Vec::new(),
        Type::Constructor(name) => vec![name.clone()],
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Field {
    Enum(Identifier),
    Struct(usize),
}

pub(crate) struct Patch {
    patches: HashMap<Identifier, Vec<Field>>,
}

impl Patch {
    pub(crate) fn get_struct<'a>(&'a self, name: &Identifier) -> impl Iterator<Item = usize> + 'a {
        self.patches
            .get(name)
            .into_iter()
            .flatten()
            .filter_map(|field| {
                if let Field::Struct(index) = field {
                    Some(*index)
                } else {
                    None
                }
            })
    }

    pub(crate) fn get_enum<'a>(
        &'a self,
        name: &Identifier,
    ) -> impl Iterator<Item = Identifier> + 'a {
        self.patches
            .get(name)
            .into_iter()
            .flatten()
            .filter_map(|field| {
                if let Field::Enum(name) = field {
                    Some(name.clone())
                } else {
                    None
                }
            })
    }
}

pub fn indirect_fields(definitions: &[Definition]) -> Patch {
    let graph: HashMap<_, _> = definitions
        .into_iter()
        .map(|def| (def.name().clone(), def.clone()))
        .collect();

    let feedback_set = minimum_feedback_arc_set(&graph);
    let mut patches: HashMap<_, Vec<_>> = HashMap::new();
    for (first, second) in feedback_set {
        let def = &graph[&first];
        patches
            .entry(first)
            .or_default()
            .extend(graph_patches(def, &second));
    }
    Patch { patches }
}

fn graph_patches(def: &Definition, to_cut: &Identifier) -> Vec<Field> {
    match def {
        Definition::Struct(Struct { fields, .. }) => fields
            .iter()
            .enumerate()
            .filter_map(|(i, field)| {
                if type_constructors(&field.typ).contains(to_cut) {
                    Some(Field::Struct(i))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>(),
        Definition::Enum(Enum { cases, .. }) => cases
            .iter()
            .filter_map(|case| {
                if type_constructors(&case.typ).contains(to_cut) {
                    Some(Field::Enum(case.name.clone()))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>(),
    }
}
