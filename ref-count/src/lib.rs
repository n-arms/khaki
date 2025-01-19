use ir::base::Program;

mod cycle_detector;
mod graph;
mod patch;

pub fn refcount(program: &mut Program) {
    let patch = cycle_detector::indirect_fields(&program.definitions);
    for def in program.definitions.iter_mut() {
        patch::patch_definition(def, &patch);
    }
    for func in program.functions.iter_mut() {
        patch::patch_function(func, &patch);
    }
}
