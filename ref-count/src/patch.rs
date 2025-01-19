use std::collections::HashSet;

use ir::{
    base::{Block, Definition, Expr, Function, MatchCase, Stmt, Storage, Type, Variable},
    hir::Identifier,
};

use crate::cycle_detector::{Field, Patch};

pub fn patch_function(function: &mut Function, patches: &Patch) {
    let seen = patch_block(&mut function.body, patches, HashSet::new());
    for (i, arg) in function.arguments.iter().enumerate() {
        if !seen.contains(arg) {
            insert_delete(arg.clone(), i, &mut function.body)
        }
    }
}

fn patch_block(
    block: &mut Block,
    patches: &Patch,
    mut seen: HashSet<Variable>,
) -> HashSet<Variable> {
    seen.insert(block.result.clone());
    for i in (0..block.stmts.len()).rev() {
        let stmt = &mut block.stmts[i];

        if let Expr::Match { cases, .. } = &mut stmt.value {
            seen = patch_match(cases, patches, seen);
        }

        let mut used = variables_used(&stmt.value);
        used.insert(stmt.var.clone());

        for var in used.difference(&seen) {
            insert_delete(var.clone(), i + 1, block);
        }
        seen.extend(used);
    }
    seen
}

// insert refcount instructions into each case of the match so that the seen of each case is the same as the return value
fn patch_match(
    cases: &mut [MatchCase],
    patches: &Patch,
    seen: HashSet<Variable>,
) -> HashSet<Variable> {
    let mut seens = Vec::new();
    let mut all_seen = HashSet::new();

    for case in cases.iter_mut() {
        let mut before_case = seen.clone();
        before_case.remove(&case.binding);
        let mut case_seen = patch_block(&mut case.body, patches, before_case);

        if case_seen.contains(&case.binding) {
            case_seen.remove(&case.binding);
        } else {
            insert_delete(case.binding.clone(), 0, &mut case.body)
        }
        case_seen.extend(seen.iter().cloned());

        all_seen.extend(case_seen.iter().cloned());
        seens.push(case_seen);
    }

    for (case, already_seen) in cases.iter_mut().zip(seens) {
        for var in all_seen.difference(&already_seen) {
            insert_delete(var.clone(), 0, &mut case.body)
        }
    }

    all_seen
}

fn insert_delete(var: Variable, index: usize, block: &mut Block) {
    match &var.typ {
        Type::Constructor(name) => {
            let function = Identifier::from(format!("delete_{}", name.name));
            block.stmts.insert(
                index,
                Stmt {
                    var: var.clone(),
                    value: Expr::DirectCall {
                        function,
                        arguments: vec![var],
                    },
                },
            );
        }
        Type::Integer => {}
    }
}

fn variables_used(expr: &Expr) -> HashSet<Variable> {
    match expr {
        Expr::Integer(_) => HashSet::new(),
        Expr::TupleAccess(name, _) | Expr::Enum { argument: name, .. } | Expr::Variable(name) => {
            HashSet::from([name.clone()])
        }
        Expr::DirectCall { arguments, .. } => arguments.into_iter().cloned().collect(),
        Expr::Tuple(elems) => elems.into_iter().cloned().collect(),
        Expr::Match { head, cases } => {
            let mut vars = HashSet::from([head.clone()]);
            /*
            for case in cases {
                let mut inner_vars = variables_used_block(&case.body);
                inner_vars.retain(|var| var != &case.binding);
                vars.extend(inner_vars);
            }
            */
            vars
        }
    }
}

fn variables_used_block(block: &Block) -> Vec<Variable> {
    let mut defined = HashSet::new();
    let mut vars = Vec::new();

    for stmt in &block.stmts {
        let mut used = variables_used(&stmt.value);
        used.retain(|var| !defined.contains(var));
        vars.extend(used);
        defined.insert(stmt.var.clone());
    }

    if !defined.contains(&block.result) {
        vars.push(block.result.clone());
    }

    vars
}

pub fn patch_definition(def: &mut Definition, patches: &Patch) {
    match def {
        Definition::Struct(def) => {
            for field in patches.get_struct(&def.name) {
                def.fields[field].storage = Storage::ReferenceCounted
            }
        }
        Definition::Enum(def) => {
            for name in patches.get_enum(&def.name) {
                for case in def.cases.iter_mut() {
                    if case.name == name {
                        case.storage = Storage::ReferenceCounted;
                    }
                }
            }
        }
    }
}
