use generator::{self as gen, Kind};
use ir::base::{Enum, Function, Program, Struct, Type};

mod generator;

pub fn gen_program(program: &Program) -> gen::Program {
    let mut builder = gen::Program::default();

    for enum_def in &program.enums {
        builder.forward_definition(Kind::Struct, enum_def.name.name.clone());
    }

    for struct_def in &program.structs {
        builder.forward_definition(Kind::Struct, struct_def.name.name.clone());
    }

    for enum_def in &program.enums {
        gen_enum(enum_def, &mut builder);
    }

    for struct_def in &program.structs {
        gen_struct(struct_def, &mut builder);
    }

    for func in &program.functions {
        gen_foward_function(func, &mut builder);
    }

    builder
}

fn gen_foward_function(func: &Function, builder: &mut gen::Program) {
    let arguments = func
        .arguments
        .iter()
        .map(|arg| (gen_type(&arg.typ), arg.name.name.clone()))
        .collect();
    builder.function(gen::Function::forward(
        gen_type(&func.typ),
        func.name.name.clone(),
        arguments,
    ))
}

fn gen_struct(struct_def: &Struct, builder: &mut gen::Program) {
    let fields = struct_def
        .fields
        .iter()
        .enumerate()
        .map(|(i, field)| format!("{} field{}", gen_type(field), i))
        .collect();
    builder.definition(Kind::Struct, struct_def.name.name.clone(), fields);
}

fn gen_enum(enum_def: &Enum, builder: &mut gen::Program) {
    let (tag_fields, union_fields) = enum_def
        .cases
        .iter()
        .map(|(case, typ)| {
            (
                format!("{}_{}", enum_def.name.name, case.name),
                format!("{} {}", gen_type(typ), case.name),
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
