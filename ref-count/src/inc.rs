use ir::base::{Block, Count, Expr, Function, Stmt};

use crate::{cycle_detector::Patch, dec::variables_used};

pub fn patch_function(function: &mut Function, patches: &Patch) {
    patch_body(&mut function.body, patches);
}

fn patch_body(body: &mut Block, patches: &Patch) {
    let mut edits = Vec::new();

    for (i, stmt) in body.stmts.iter_mut().enumerate() {
        if let Expr::Match { cases, .. } = &mut stmt.value {
            for case in cases {
                patch_body(&mut case.body, patches);
            }
        }

        for var in variables_used(&stmt.value) {
            edits.push((i, var))
        }
    }

    for (index, var) in edits.into_iter().rev() {
        body.stmts.insert(
            index,
            Stmt {
                var,
                value: Expr::RefCount(Count::Increment),
            },
        );
    }
}
