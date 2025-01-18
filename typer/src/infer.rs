use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::substitute::Subst;
use crate::{Error, Result};
use im::HashMap;
use ir::hir::{self};
use ir::parsed::{
    self, Enum, Expr, Function, LambdaSet, Pattern, Program, Span, Type, VariableCell,
};
use ir::union_find::UnionFind;

#[derive(Clone)]
pub(crate) struct Env {
    enums: HashMap<hir::Identifier, Enum>,
    variables: HashMap<hir::Identifier, Variable>,
    lambda_sets: Rc<RefCell<UnionFind<LambdaSet>>>,
    fresh_variables: Rc<Cell<usize>>,
}

#[derive(Clone)]
pub(crate) struct Variable {
    name: parsed::Identifier,
    typ: Scheme,
}

#[derive(Clone)]
pub(crate) enum Scheme {
    Forall(Vec<parsed::Identifier>, Type),
    Type(Type),
}

impl Env {
    fn new(lambda_sets: usize) -> Self {
        Self {
            enums: HashMap::new(),
            variables: HashMap::new(),
            lambda_sets: Rc::new(RefCell::new(UnionFind::new_with(lambda_sets))),
            fresh_variables: Rc::new(Cell::new(0)),
        }
    }

    fn define_enum(&mut self, def: Enum) -> Result<()> {
        if let Some(existing) = self.enums.get(&def.name.name) {
            Err(Error::DuplicateEnumDefinition(
                def.name.clone(),
                existing.name.clone(),
            ))
        } else {
            self.enums.insert(def.name.name.clone(), def);
            Ok(())
        }
    }

    fn lookup_enum(&self, name: &parsed::Identifier) -> Result<&Enum> {
        self.enums
            .get(&name.name)
            .ok_or_else(|| Error::UnknownEnum(name.clone()))
    }

    #[must_use]
    fn define_variable(mut self, name: parsed::Identifier, typ: Type) -> Self {
        self.variables.insert(
            name.name.clone(),
            Variable {
                name,
                typ: Scheme::Type(typ),
            },
        );
        self
    }

    fn define_scheme(
        &mut self,
        name: parsed::Identifier,
        generics: Vec<parsed::Identifier>,
        typ: Type,
    ) {
        self.variables.insert(
            name.name.clone(),
            Variable {
                name,
                typ: Scheme::Forall(generics, typ),
            },
        );
    }

    fn lookup_variable(&self, name: &parsed::Identifier) -> Result<&Variable> {
        self.variables
            .get(&name.name)
            .ok_or_else(|| Error::UnknownVariable(name.clone()))
    }

    fn unify_set(&self, set1: LambdaSet, set2: LambdaSet) {
        self.lambda_sets.as_ref().borrow_mut().merge(set1, set2);
    }

    fn unification_var(&self, span: Span) -> VariableCell {
        let token = self.fresh_variables.get();
        self.fresh_variables.set(token + 1);
        let name = parsed::Identifier::new(format!("uni_{}", token), span);
        VariableCell::new(name)
    }
}

pub fn infer_program(program: &mut Program, lambda_sets: usize) -> Result<()> {
    let mut env = Env::new(lambda_sets);
    for (_, def) in program.enums.iter_mut() {
        infer_enum(def, &mut env)?;
    }
    for func in program.functions.iter() {
        define_function(func, &mut env)?;
    }
    for func in program.functions.iter_mut() {
        infer_function(func, env.clone())?;
    }
    Ok(())
}

fn define_function(func: &Function, env: &mut Env) -> Result<()> {
    if let Ok(Variable { name, .. }) = env.lookup_variable(&func.name) {
        Err(Error::DuplicateFunctionDefinition(
            func.name.clone(),
            name.clone(),
        ))
    } else {
        env.define_scheme(func.name.clone(), func.generics.clone(), func.typ());
        Ok(())
    }
}

fn infer_function(func: &mut Function, mut env: Env) -> Result<()> {
    for arg in &mut func.arguments {
        env = check_pattern(&mut arg.binding, &arg.typ, env)?;
    }
    check_expr(&mut func.body, &func.result, env)
}

fn check_expr(expr: &mut Expr, typ: &Type, mut env: Env) -> Result<()> {
    match (expr, typ) {
        (Expr::Integer(_, _), Type::Integer(_)) => Ok(()),
        (Expr::Variable(name, var_generics, var_typ), typ) => {
            let Variable {
                name: env_name,
                typ: scheme,
            } = env.lookup_variable(&name)?;
            match scheme {
                Scheme::Forall(generics, generic_typ) => todo!(),
                Scheme::Type(env_typ) => {
                    let env_typ = env_typ.clone();
                    *var_typ = Some(env_typ.clone());
                    unify(&env_typ, typ, name.span, &mut env)?;
                }
            }
            Ok(())
        }
        (
            Expr::Function {
                captures,
                arguments,
                result,
                body,
                set,
                name,
                span,
            },
            Type::Function(arg_types, res_type, type_set, type_span),
        ) => {
            env.unify_set(set.clone(), type_set.clone());

            let mut inner = env.clone();
            for (arg, arg_typ) in arguments.iter_mut().zip(arg_types) {
                if let Some(expected_typ) = arg.typ.as_ref() {
                    unify(arg_typ, expected_typ, arg.span, &mut inner)?;
                } else {
                    arg.typ = Some(arg_typ.clone());
                }
                inner = check_pattern(&mut arg.binding, &arg_typ, inner)?;
            }
            if let Some(defined_result) = result.as_ref() {
                unify(&res_type, defined_result, defined_result.span(), &mut env)?;
            } else {
                *result = Some(res_type.as_ref().clone());
            }
            check_expr(body.as_mut(), res_type.as_ref(), inner)
        }
        (expr, typ) => {
            let inferred = infer_expr(expr, env.clone())?;
            unify(&typ, &inferred, expr.span(), &mut env)
        }
    }
}

fn infer_expr(expr: &mut Expr, mut env: Env) -> Result<Type> {
    match expr {
        Expr::Integer(_, span) => Ok(Type::Integer(*span)),
        Expr::Variable(name, var_generics, _typ) => {
            let Variable {
                name: env_name,
                typ: scheme,
            } = env.lookup_variable(name)?;
            let typ = match scheme {
                Scheme::Forall(_, _) => todo!(),
                Scheme::Type(typ) => typ.clone(),
            };
            *_typ = Some(typ.clone());
            Ok(typ)
        }
        Expr::FunctionCall {
            function,
            set,
            arguments,
            span,
        } => {
            let result_typ = Type::Unification(env.unification_var(*span));

            let arg_types = arguments
                .iter_mut()
                .map(|arg| infer_expr(arg, env.clone()))
                .collect::<Result<Vec<_>>>()?;

            let typ = Type::Function(arg_types, Box::new(result_typ.clone()), set.clone(), *span);

            check_expr(function.as_mut(), &typ, env)?;

            Ok(result_typ)
        }
        Expr::Function {
            captures,
            arguments,
            result,
            body,
            set,
            name,
            span,
        } => {
            let mut arg_types = Vec::new();
            for arg in arguments.iter_mut() {
                let typ = if let Some(arg_typ) = arg.typ.as_ref() {
                    env = check_pattern(&mut arg.binding, arg_typ, env)?;
                    arg_typ.clone()
                } else {
                    let (new_env, typ) = infer_pattern(&mut arg.binding, env)?;
                    env = new_env;
                    arg.typ = Some(typ.clone());
                    typ
                };
                arg_types.push(typ);
            }
            let result_typ = if let Some(result_typ) = result.as_ref() {
                check_expr(body.as_mut(), result_typ, env)?;
                result_typ.clone()
            } else {
                let typ = infer_expr(body.as_mut(), env)?;
                *result = Some(typ.clone());
                typ
            };
            Ok(Type::Function(
                arg_types,
                Box::new(result_typ),
                set.clone(),
                *span,
            ))
        }
        Expr::Tuple(elems, span) => {
            let elem_types = elems
                .iter_mut()
                .map(|elem| infer_expr(elem, env.clone()))
                .collect::<Result<_>>()?;
            Ok(Type::Tuple(elem_types, *span))
        }
        Expr::TupleAccess(tuple, field, span) => {
            let typ = infer_expr(tuple.as_mut(), env)?.normalize();
            if let Type::Tuple(elem_types, type_span) = typ {
                Ok(elem_types[*field].clone())
            } else {
                panic!("tuple access elem type is not a tuple {:?}", typ)
            }
        }
        Expr::Enum {
            typ,
            tag,
            argument,
            span,
        } => {
            let def = env.lookup_enum(typ)?;
            let (expected, generics) = variant_type(def, tag, &mut env.clone())?;
            check_expr(argument.as_mut(), &expected, env)?;
            Ok(Type::Constructor(typ.clone(), generics, *span))
        }
        Expr::Match { head, cases, span } => {
            let head_typ = infer_expr(head.as_mut(), env.clone())?.normalize();
            let Type::Constructor(typ_name, generics, typ_span) = head_typ else {
                panic!("match head is not an enum {:?}", head_typ)
            };
            let def = env.lookup_enum(&typ_name)?;
            let mut result_types = Vec::new();
            for case in cases.iter_mut() {
                let mut inner = env.clone();
                let (binding_typ, binding_generics) = variant_type(def, &case.variant, &mut inner)?;
                for (binding, typ) in binding_generics.iter().zip(generics.iter()) {
                    unify(typ, binding, case.span, &mut inner)?;
                }
                inner = check_pattern(&mut case.binding, &binding_typ, inner)?;
                let result = infer_expr(&mut case.body, inner)?;
                result_types.push(result);
            }
            for results in result_types.windows(2) {
                unify(&results[0], &results[1], *span, &mut env)?;
            }
            Ok(result_types[0].clone())
        }
    }
}

fn variant_type(def: &Enum, tag: &parsed::Identifier, env: &mut Env) -> Result<(Type, Vec<Type>)> {
    let case = def
        .cases
        .iter()
        .find(|case| &case.tag == tag)
        .ok_or_else(|| Error::UndefinedEnumVariant(def.clone(), tag.clone()))?;
    let generalized = case.typ.clone();
    let mut unis = Vec::new();
    let subst = Subst::new(def.generics.iter().map(|name| {
        let typ = Type::Unification(env.unification_var(name.span));
        unis.push(typ.clone());
        (name.clone(), typ)
    }));
    Ok((subst.typ(&generalized), unis))
}

fn unify(given: &Type, expected: &Type, usage: Span, env: &mut Env) -> Result<()> {
    match (given, expected) {
        (Type::Integer(_), Type::Integer(_)) => Ok(()),
        (Type::Unification(cell), expected) => {
            if cell.is_some() {
                unify(&*cell.get(), expected, usage, env)
            } else {
                cell.set(expected.clone());
                Ok(())
            }
        }
        (given, Type::Unification(cell)) => {
            if cell.is_some() {
                unify(given, &*cell.get(), usage, env)
            } else {
                cell.set(given.clone());
                Ok(())
            }
        }
        (Type::Rigid(name1), Type::Rigid(name2)) => {
            if name1 == name2 {
                Ok(())
            } else {
                Err(Error::RigidTypeMismatch(
                    name1.clone(),
                    name2.clone(),
                    usage.clone(),
                ))
            }
        }
        (Type::Function(args1, res1, set1, span1), Type::Function(args2, res2, set2, span2)) => {
            assert_eq!(args1.len(), args2.len());
            for (arg1, arg2) in args1.iter().zip(args2) {
                unify(arg1, arg2, usage, env)?;
            }
            env.unify_set(set1.clone(), set2.clone());
            unify(res1, res2, usage, env)
        }
        (Type::Tuple(elems1, _), Type::Tuple(elems2, _)) => {
            assert_eq!(elems1.len(), elems2.len());
            for (elem1, elem2) in elems1.iter().zip(elems2) {
                unify(elem1, elem2, usage, env)?;
            }
            Ok(())
        }
        (Type::Constructor(name1, args1, _), Type::Constructor(name2, args2, _)) => {
            if name1 == name2 {
                for (arg1, arg2) in args1.iter().zip(args2) {
                    unify(arg1, arg2, usage, env)?;
                }
                Ok(())
            } else {
                Err(Error::ConstructorMismatch(
                    name1.clone(),
                    args1.clone(),
                    name2.clone(),
                    args2.clone(),
                    usage.clone(),
                ))
            }
        }
        (given, expected) => Err(Error::TypeMismatch(given.clone(), expected.clone())),
    }
}

fn infer_pattern(pattern: &mut Pattern, mut env: Env) -> Result<(Env, Type)> {
    match pattern {
        Pattern::Variable(name, typ) => {
            let uni = env.unification_var(name.span);
            let uni_typ = Type::Unification(uni);
            *typ = Some(uni_typ.clone());
            Ok((env.define_variable(name.clone(), uni_typ.clone()), uni_typ))
        }
        Pattern::Tuple(elems, span) => {
            let mut types = Vec::new();
            for elem in elems.iter_mut() {
                let (new_env, typ) = infer_pattern(elem, env)?;
                env = new_env;
                types.push(typ);
            }
            Ok((env, Type::Tuple(types, *span)))
        }
    }
}

fn check_pattern(pattern: &mut Pattern, typ: &Type, mut env: Env) -> Result<Env> {
    match (pattern, typ) {
        (Pattern::Tuple(patterns, _), Type::Tuple(typs, _)) => {
            assert_eq!(patterns.len(), typs.len());
            // TODO: better error handling here
            for (pattern, typ) in patterns.iter_mut().zip(typs) {
                env = check_pattern(pattern, typ, env)?;
            }
            Ok(env)
        }
        (Pattern::Variable(name, labeled_typ), typ) => {
            assert!(labeled_typ.is_none());
            *labeled_typ = Some(typ.clone());
            Ok(env.define_variable(name.clone(), typ.clone()))
        }
        (Pattern::Tuple(patterns, span), typ) => Err(Error::InappropriateTuplePattern(
            patterns.clone(),
            span.clone(),
            typ.clone(),
        )),
    }
}

fn infer_enum(def: &mut Enum, env: &mut Env) -> Result<()> {
    env.define_enum(def.clone())
}
