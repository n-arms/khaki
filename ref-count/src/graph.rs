use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    iter,
};

use ir::base::{Enum, Identifier, Type};

pub trait Graph {
    type Node: Eq + Hash + Clone;

    fn children(&self, node: &Self::Node) -> Vec<Self::Node>;
    fn nodes(&self) -> Vec<Self::Node>;
}

// approximate the [minimum feedback arc set](https://en.wikipedia.org/wiki/Feedback_arc_set) such that if the returned edges are removed the graph, the graph will be acyclic.
// this algorithm is very imperfect, doing a DFS over the graph and adding each back edge into the feedback arc set.
pub fn minimum_feedback_arc_set<G: Graph>(graph: &G) -> Vec<(G::Node, G::Node)> {
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();

    graph
        .nodes()
        .into_iter()
        .flat_map(|node| dfs(graph, &node, &mut visiting, &mut visited))
        .collect()
}

fn dfs<G: Graph>(
    graph: &G,
    current: &G::Node,
    visiting: &mut HashSet<G::Node>,
    visited: &mut HashSet<G::Node>,
) -> Vec<(G::Node, G::Node)> {
    if visited.contains(current) {
        return Vec::new();
    }
    visiting.insert(current.clone());

    let mut feedback_set = Vec::new();
    for child in graph.children(current) {
        if visiting.contains(&child) {
            feedback_set.push((current.clone(), child.clone()));
        } else {
            feedback_set.extend(dfs(graph, &child, visiting, visited));
        }
    }

    visiting.remove(current);
    visited.insert(current.clone());

    feedback_set
}
