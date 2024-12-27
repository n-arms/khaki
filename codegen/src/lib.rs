use generator::Generator;
use ir::parsed::Function;

mod generator;

pub fn program(functions: Vec<Function>) -> String {
    let mut gen = Generator::default();

    preamble(&mut gen);

    for func in &functions {
        forward_function(func, &mut gen);
    }

    for func in functions {
        function(func, &mut gen);
    }

    gen.generate()
}

fn preamble(gen: &mut Generator) {
    todo!()
}

fn forward_function(func: &Function, gen: &mut Generator) {
    todo!()
}

fn function(func: Function, gen: &mut Generator) {
    todo!()
}
