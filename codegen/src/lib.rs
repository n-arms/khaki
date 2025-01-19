use generator::{self as gen, commas_with, Kind};
use ir::base::{
    Definition, Enum, Expr, Function, MatchCase, Program, Stmt, Struct, Type, Variable,
};

mod generator;

pub fn gen_program(program: &Program) -> gen::Program {
    let mut builder = gen::Program::default();

    for def in &program.definitions {
        let name = match def {
            Definition::Struct(def) => &def.name,
            Definition::Enum(def) => &def.name,
        }
        .name
        .clone();
        builder.forward_definition(Kind::Struct, name);
    }

    for def in &program.definitions {
        match def {
            Definition::Struct(def) => gen_struct(def, &mut builder),
            Definition::Enum(def) => gen_enum(def, &mut builder),
        }
    }

    for func in &program.functions {
        gen_foward_function(func, &mut builder);
    }

    for func in &program.functions {
        gen_function(func, &mut builder);
    }

    builder
}

fn gen_function(func: &Function, builder: &mut gen::Program) {
    let mut header = forward_function(func);
    let mut block = gen::Block::default();

    for stmt in &func.body.stmts {
        gen_stmt(stmt, &mut block);
    }

    block.line(format!("return {}", func.body.result));
    header.body = Some(block);
    builder.function(header);
}

fn gen_stmt(stmt: &Stmt, block: &mut gen::Block) {
    if let Expr::Match { head, cases } = &stmt.value {
        gen_match(&stmt.var, head, cases, block);
    } else if let Expr::RefCount(count) = &stmt.value {
        todo!()
    } else {
        let expr_format = gen_expr(&stmt.value, &stmt.var.typ);
        block.line(format!(
            "{} {} = {}",
            gen_type(&stmt.var.typ),
            stmt.var,
            expr_format
        ));
    }
}

fn gen_match(var: &Variable, head: &Variable, cases: &[MatchCase], block: &mut gen::Block) {
    block.line(format!("{} {}", gen_type(&var.typ), var));
    let mut case_block = gen::Block::default();
    let Type::Constructor(enum_name) = head.typ.clone() else {
        unreachable!()
    };
    for case in cases {
        let mut inner_block = gen::Block::default();
        inner_block.line(format!(
            "{} {} = {}.value.{}",
            gen_type(&case.binding.typ),
            case.binding,
            head,
            case.variant.name
        ));
        for stmt in &case.body.stmts {
            gen_stmt(stmt, &mut inner_block);
        }
        inner_block.line(format!("{} = {}", var, case.body.result));
        inner_block.line("break".into());
        case_block.block(
            format!("case {}_{}:", enum_name.name, case.variant.name),
            inner_block,
        );
    }
    block.block(format!("switch ({}.tag)", head), case_block);
}

fn gen_expr(expr: &Expr, typ: &Type) -> String {
    match expr {
        Expr::Integer(int) => int.to_string(),
        Expr::Variable(var) => var.to_string(),
        Expr::DirectCall {
            function,
            arguments,
        } => {
            format!(
                "{}({})",
                function.name,
                commas_with(arguments, |arg| arg.to_string())
            )
        }
        Expr::Tuple(elems) => {
            format!(
                "({}) {{{}}}",
                gen_type(typ),
                commas_with(elems, |elem| elem.to_string())
            )
        }
        Expr::TupleAccess(tuple, field) => {
            format!("{}.field{}", tuple, field)
        }
        Expr::Enum { typ, tag, argument } => {
            format!(
                "(struct {}) {{ {}_{}, {{ .{} = {} }} }}",
                typ.name, typ.name, tag.name, tag.name, argument
            )
        }
        Expr::Match { .. } => unreachable!(),
        Expr::RefCount(_) => unreachable!(),
    }
}

fn forward_function(func: &Function) -> gen::Function {
    let arguments = func
        .arguments
        .iter()
        .map(|arg| (gen_type(&arg.typ), arg.name.name.clone()))
        .collect();
    gen::Function::forward(
        gen_type(&func.body.result.typ),
        func.name.name.clone(),
        arguments,
    )
}

fn gen_foward_function(func: &Function, builder: &mut gen::Program) {
    builder.function(forward_function(func));
}

fn gen_struct(struct_def: &Struct, builder: &mut gen::Program) {
    let fields = struct_def
        .fields
        .iter()
        .enumerate()
        .map(|(i, field)| format!("{} field{}", gen_type(&field.typ), i))
        .collect();
    builder.definition(Kind::Struct, struct_def.name.name.clone(), fields);
}

fn gen_enum(enum_def: &Enum, builder: &mut gen::Program) {
    let (tag_fields, union_fields) = enum_def
        .cases
        .iter()
        .map(|case| {
            (
                format!("{}_{}", enum_def.name.name, case.name.name),
                format!("{} {}", gen_type(&case.typ), case.name.name),
            )
        })
        .unzip();
    let struct_fields = vec![
        format!("enum {}_tag tag", enum_def.name.name),
        format!("union {}_value value", enum_def.name.name),
    ];
    builder.definition(
        Kind::Enum,
        format!("{}_tag", enum_def.name.name),
        tag_fields,
    );
    builder.definition(
        Kind::Union,
        format!("{}_value", enum_def.name.name),
        union_fields,
    );
    builder.definition(Kind::Struct, enum_def.name.name.clone(), struct_fields);
}

fn gen_type(typ: &Type) -> String {
    match typ {
        Type::Constructor(name) => format!("struct {}", name.name),
        Type::Integer => String::from("int"),
    }
}
