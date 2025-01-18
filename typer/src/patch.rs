use ir::{
    hir::{self, LambdaSet},
    parsed::{Argument, Enum, EnumCase, Expr, Function, Pattern, Program, Type},
    union_find::UnionFind,
};

pub(crate) struct Patcher {
    lambda_sets: UnionFind<LambdaSet>,
    fresh: usize,
}

impl Patcher {
    pub(crate) fn new(lambda_sets: UnionFind<LambdaSet>) -> Self {
        Self {
            lambda_sets,
            fresh: 0,
        }
    }

    fn fresh_var(&mut self, prefix: &str) -> hir::Identifier {
        let token = self.fresh;
        self.fresh += 1;
        hir::Identifier::from(format!("{prefix}_{token}"))
    }

    fn lambda_set_root(&mut self, set: LambdaSet) -> LambdaSet {
        self.lambda_sets.root(set)
    }
}

pub fn patch_program(program: &Program, patcher: &mut Patcher) -> hir::Program {
    let functions = program
        .functions
        .iter()
        .map(|func| patch_function(func, patcher))
        .collect();
    let enums = program
        .enums
        .iter()
        .map(|(_, def)| patch_enum(def, patcher))
        .collect();
    hir::Program { functions, enums }
}

fn patch_enum(def: &Enum, patcher: &mut Patcher) -> (hir::Identifier, hir::Enum) {
    let cases = def
        .cases
        .iter()
        .map(|case| (case.tag.name.clone(), patch_typ(&case.typ, patcher)))
        .collect();
    let def = hir::Enum {
        name: def.name.name.clone(),
        cases,
    };
    (def.name.clone(), def)
}

fn patch_function(func: &Function, patcher: &mut Patcher) -> hir::Function {
    let mut lets = Vec::new();
    let arguments = func
        .arguments
        .iter()
        .map(|arg| patch_arg(arg, patcher, &mut lets))
        .collect();
    assert_eq!(lets.len(), 0);
    let generics = func
        .generics
        .iter()
        .map(|generic| generic.name.clone())
        .collect();
    let result = patch_typ(&func.result, patcher);
    let body = patch_expr(&func.body, patcher);
    hir::Function {
        name: func.name.name.clone(),
        arguments,
        generics,
        result,
        body,
    }
}

fn patch_arg(

    patcher: &mut Patcher,
    lets: &mut Vec<(hir::Identifier, hir::Type, hir::Expr)>,
) -> hir::Argument {
    let arg_name = patcher.fresh_var("arg");
    let typ = patch_typ(&arg.typ, patcher);
    lets.extend(bind_pattern(
        hir::Expr::Variable {
            name: arg_name.clone(),
            generics: Vec::new(),
            typ: Some(typ.clone()),
        },
        &arg.binding,
        patcher,
    ));
    hir::Argument {
        name: arg_name,
        typ,
    }
}

fn patch_typ(typ: &Type, patcher: &mut Patcher) -> hir::Type {
    match typ {
        Type::Integer(_) => hir::Type::Integer,
        Type::Unification(cell) => {
            if cell.is_some() {
                patch_typ(&*cell.get(), patcher)
            } else {
                panic!("unresolved type")
            }
        }
        Type::Rigid(name) => hir::Type::Variable(name.name.clone()),
        Type::Function(args, res, set, _) => hir::Type::Function(
            args.iter().map(|arg| patch_typ(arg, patcher)).collect(),
            Box::new(patch_typ(&res, patcher)),
            patcher.lambda_set_root(set.clone()),
        ),
        Type::Tuple(elems, _) => {
            hir::Type::Tuple(elems.iter().map(|elem| patch_typ(elem, patcher)).collect())
        }
        Type::Constructor(name, args, _) => {
            if args.is_empty() {
                hir::Type::Constructor(name.name.clone())
            } else {
                todo!()
            }
        }
    }
}

fn bind_pattern(
    expr: hir::Expr,
    pattern: &Pattern,
    patcher: &mut Patcher,
) -> Vec<(hir::Identifier, hir::Type, hir::Expr)> {
    match pattern {
        Pattern::Variable(name, typ) => {
            vec![(
                name.name.clone(),
                patch_typ(typ.as_ref().unwrap(), patcher),
                expr,
            )]
        }
        Pattern::Tuple(elems, _) => elems
            .iter()
            .enumerate()
            .flat_map(|(field, pattern)| {
                bind_pattern(
                    hir::Expr::TupleAccess(Box::new(expr.clone()), field),
                    pattern,
                    patcher,
                )
            })
            .collect(),
    }
}

fn patch_expr(expr: &Expr, patcher: &mut Patcher) -> hir::Expr {
    match expr {
        Expr::Integer(int, _) => hir::Expr::Integer(*int),
        Expr::Variable(name, generics, typ) => hir::Expr::Variable {
            name: name.name.clone(),
            generics: generics
                .iter()
                .map(|generic| patch_typ(generic, patcher))
                .collect(),
            typ: Some(patch_typ(typ.as_ref().unwrap(), patcher)),
        },
        Expr::FunctionCall {
            function,
            set,
            arguments,
            span,
        } => hir::Expr::FunctionCall {
            function: Box::new(patch_expr(function.as_ref(), patcher)),
            set: patcher.lambda_set_root(set.clone()),
            arguments: arguments
                .iter()
                .map(|arg| patch_expr(arg, patcher))
                .collect(),
        },
        Expr::Function {
            captures,
            arguments,
            result,
            body,
            set,
            name,
            span,
        } => {
            let hir_captures = captures.iter().map(|cap| hir::Argument {
                name: cap.name.name.clone(),
                typ: patch_typ(cap.typ, patcher),
            }).collect();
            let hir_arguments = arguments.iter().map(|arg| hir::Argument {
                name: arg.name,
                typ: todo!(),
            }).collect();
            hir::Expr::Function { captures: hir_captures, arguments: , result: , body: , set: , name:  }
        },
        Expr::Tuple(_, _) => todo!(),
        Expr::TupleAccess(_, _, _) => todo!(),
        Expr::Enum {
            typ,
            tag,
            argument,
            span,
        } => todo!(),
        Expr::Match { head, cases, span } => todo!(),
    }
}
