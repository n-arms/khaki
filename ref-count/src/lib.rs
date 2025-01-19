use ir::base::Program;

mod cycle_detector;
mod dec;
mod graph;
mod inc;

pub fn refcount(program: &mut Program) {
    let patch = cycle_detector::indirect_fields(&program.definitions);
    for def in program.definitions.iter_mut() {
        dec::patch_definition(def, &patch);
    }
    for func in program.functions.iter_mut() {
        dec::patch_function(func, &patch);
        inc::patch_function(func, &patch);
    }
}
