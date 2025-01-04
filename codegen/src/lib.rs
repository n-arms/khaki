use std::{any::Any, collections::HashMap};

use generator::{self as gen, Kind};
use ir::parsed::{Enum, Expr, Function, Identifier, Program, Type};

mod generator;

struct Env {
    pub prog: gen::Program,
    names: usize,
    tuples: HashMap<Vec<Type>, String>,
    enums: HashMap<Identifier, Enum>,
}

impl Env {
    pub fn fresh_name(&mut self, prefix: &str) -> String {
        let id = self.names;
        self.names += 1;
        format!("{}_{}", prefix, id)
    }

    pub fn tuple_name(&mut self, tuple: &[Type]) -> String {
        if let Some(name) = self.tuples.get(tuple) {
            name.clone()
        } else {
            let name = self.fresh_name("tuple");

            tuple_definition(name.clone(), tuple, self);

            self.tuples.insert(tuple.to_vec(), name.clone());

            name
        }
    }
}

pub fn program(prog: &Program) -> String {
    generate_program(prog).generate()
}

fn generate_program(prog: &Program) -> gen::Program {
    let builder = gen::Program::default();
    let mut env = Env {
        prog: builder,
        names: 0,
        tuples: HashMap::new(),
        enums: prog.enums.clone(),
    };
    for def in prog.enums.values() {
        enum_definition(def, &mut env);
    }

    for func in &prog.functions {
        let def = forward_function(func, &mut env);
        env.prog.function(def);
    }

    for func in &prog.functions {
        function(func, &mut env);
    }

    println!("{:#?}", env.prog);

    env.prog
}

fn tuple_definition(name: String, tuple: &[Type], env: &mut Env) {
    let fields = tuple
        .iter()
        .enumerate()
        .map(|(i, elem)| format!("{} field{}", typ_to_string(elem, env), i))
        .collect();
    env.prog.definition(Kind::Struct, name, fields);
}

fn enum_definition(def: &Enum, env: &mut Env) {
    let tag_cases = def
        .cases
        .iter()
        .map(|(case, _)| format!("{}_{}", def.name.name, case.name))
        .collect();
    env.prog
        .definition(Kind::Enum, format!("{}_tag", def.name.name), tag_cases);

    let union_fields = def
        .cases
        .iter()
        .map(|(case, typ)| format!("{} {}", typ_to_string(typ, env), case.name))
        .collect();
    env.prog.definition(
        Kind::Union,
        format!("{}_value", def.name.name),
        union_fields,
    );

    env.prog.definition(
        Kind::Struct,
        def.name.name.clone(),
        vec![
            format!("enum {}_tag tag", def.name.name),
            format!("union {}_value value", def.name.name),
        ],
    );
}

fn forward_function(func: &Function, env: &mut Env) -> gen::Function {
    let args = func
        .arguments
        .iter()
        .map(|arg| (typ_to_string(&arg.typ, env), arg.name.name.clone()))
        .collect();
    gen::Function::forward(
        typ_to_string(&func.result, env),
        func.name.name.clone(),
        args,
    )
}

fn function(func: &Function, env: &mut Env) {
    let mut output = forward_function(func, env);
    let mut block = gen::Block::default();

    let result = expr(&func.body, &mut block, env);
    block.line(format!("return {}", result));

    output.body = Some(block);
    env.prog.function(output);
}

fn expr(to_gen: &Expr, block: &mut gen::Block, env: &mut Env) -> String {
    match to_gen {
        Expr::Integer(int) => int.to_string(),
        Expr::Variable { name, .. } => name.name.clone(),
        Expr::FunctionCall {
            function,
            arguments,
            ..
        } => {
            let mut output = format!("{}(", function.name);

            let mut first = true;

            for arg in arguments {
                if first {
                    first = false;
                } else {
                    output.push_str(", ");
                }
                let arg = expr(arg, block, env);
                output.push_str(&arg);
            }
            output.push(')');
            output
        }
        Expr::Function {
            captures,
            arguments,
            result,
            body,
            set,
            name,
        } => todo!(),
        Expr::Tuple(elems) => {
            let typ = elems.iter().map(|e| e.typ()).collect::<Vec<_>>();
            let name = env.tuple_name(&typ);

            let mut output = format!("(struct {}) {{", name);

            let mut first = true;
            for elem in elems {
                if first {
                    first = false;
                } else {
                    output.push_str(",");
                }
                output.push(' ');
                output.push_str(&expr(elem, block, env));
            }
            output.push_str(" }");
            output
        }
        Expr::TupleAccess(tuple, field) => {
            format!("({}).field{}", expr(&tuple, block, env), field)
        }
        Expr::Enum { typ, tag, argument } => {
            format!(
                "(struct {}) {{ {}_{}, {{ .{} = {} }} }}",
                typ.name,
                typ.name,
                tag.name,
                tag.name,
                expr(&argument, block, env)
            )
        }
        Expr::Match { head, cases } => {
            let Type::Constructor(head_typ) = head.typ() else {
                unreachable!()
            };
            let enum_def = env.enums[&head_typ].clone();
            let case_typ = cases[0].body.typ();
            let result = env.fresh_name("var");
            block.line(format!("{} {result}", typ_to_string(&case_typ, env)));

            let head_name = env.fresh_name("var");
            let head_expr = expr(head, block, env);
            block.line(format!(
                "struct {} {head_name} = {head_expr}",
                head_typ.name
            ));

            let mut switch = gen::Block::default();

            for case in cases {
                let mut local = gen::Block::default();
                let binding_typ = enum_def.variant_type(&case.variant);
                let binding_typ_name = typ_to_string(binding_typ, env);
                local.line(format!(
                    "{binding_typ_name} {} = ({head_name}).value.{}",
                    case.binding.name, case.variant.name
                ));
                let expr_name = expr(&case.body, &mut local, env);
                local.line(format!("{result} = {expr_name}",));
                local.line(String::from("break"));
                switch.block(
                    format!("case {}_{}:", head_typ.name, case.variant.name),
                    local,
                );
            }
            block.block(format!("switch (({head_name}).tag)"), switch);
            result
        }
    }
}

fn typ_to_string(typ: &Type, env: &mut Env) -> String {
    match typ {
        Type::Integer => String::from("int"),
        Type::Variable(var) => panic!("codegen with unresolved generic {:?}", var),
        Type::Function(args, result, set) => String::from("void*"),
        Type::Tuple(elems) => {
            let name = env.tuple_name(elems);
            format!("struct {}", name)
        }
        Type::Constructor(name) => format!("struct {}", name.name),
    }
}
