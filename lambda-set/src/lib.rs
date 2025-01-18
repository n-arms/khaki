use defunc::defunc_program;
use gather::gather_program;
use ir::hir::Program;

mod defunc;
mod gather;
mod pool;

pub fn defunctionalize_program(program: &mut Program) {
    let pools = gather_program(program);
    defunc_program(program, &pools)
}
