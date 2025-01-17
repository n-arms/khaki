use std::collections::HashMap;

use ir::parsed::{Identifier, Type};

pub struct Subst {
    rigid_vars: HashMap<Identifier, Type>,
}

impl Subst {
    pub fn new(map: impl IntoIterator<Item = (Identifier, Type)>) -> Self {
        Self {
            rigid_vars: map.into_iter().collect(),
        }
    }

    pub fn typ(&self, to_subst: &Type) -> Type {
        match to_subst {
            Type::Integer(_) => to_subst.clone(),
            Type::Unification(cell) => {
                if cell.is_some() {
                    self.typ(&cell.get())
                } else {
                    to_subst.clone()
                }
            }
            Type::Rigid(rigid_var) => self
                .rigid_vars
                .get(rigid_var)
                .cloned()
                .unwrap_or_else(|| to_subst.clone()),
            Type::Function(args, res, set, span) => Type::Function(
                args.iter().map(|arg| self.typ(arg)).collect(),
                Box::new(self.typ(&res)),
                set.clone(),
                *span,
            ),
            Type::Tuple(elems, span) => {
                Type::Tuple(elems.iter().map(|arg| self.typ(arg)).collect(), *span)
            }
            Type::Constructor(name, args, span) => Type::Constructor(
                name.clone(),
                args.iter().map(|arg| self.typ(arg)).collect(),
                *span,
            ),
        }
    }
}
