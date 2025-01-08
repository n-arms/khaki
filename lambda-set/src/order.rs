use std::collections::HashSet;

use im::HashMap;
use ir::base::{Definition, Enum, Identifier, Struct, Type};

pub(crate) fn sort_definitions(definitions: Vec<Definition>) -> Vec<Definition> {
    let map: HashMap<Identifier, &Definition> = definitions
        .iter()
        .map(|def| (def.name().clone(), def))
        .collect();

    let mut visited = HashSet::new();

    let mut sorted = Vec::new();

    for def in &definitions {
        dfs(def.name().clone(), &map, &mut visited, &mut sorted);
    }

    sorted
}

fn dfs(
    node: Identifier,
    children: &HashMap<Identifier, &Definition>,
    visited: &mut HashSet<Identifier>,
    order: &mut Vec<Definition>,
) {
    if visited.contains(&node) {
        return;
    }
    visited.insert(node.clone());
    let def = children[&node];

    for child in dependencies(def) {
        dfs(child, children, visited, order);
    }

    order.push(def.clone());
}

fn dependencies(def: &Definition) -> Vec<Identifier> {
    match def {
        Definition::Struct(def) => struct_dependencies(def),
        Definition::Enum(def) => enum_dependencies(def),
    }
}

fn enum_dependencies(def: &Enum) -> Vec<Identifier> {
    def.cases
        .iter()
        .flat_map(|(_, field)| typ_dependencies(field))
        .collect()
}

fn struct_dependencies(strukt: &Struct) -> Vec<Identifier> {
    strukt.fields.iter().flat_map(typ_dependencies).collect()
}

fn typ_dependencies(typ: &Type) -> Vec<Identifier> {
    match typ {
        Type::Constructor(var) => vec![var.clone()],
        Type::Integer => vec![],
    }
}
