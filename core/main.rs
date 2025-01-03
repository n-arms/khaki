use parser::parse_program;

use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    let mut text = String::new();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if line == "" {
            let parsed = parse_program(&text).unwrap();
            let flat = flatten::program(parsed);
            for func in &flat {
                println!("{:?}", func);
            }
            let gen = codegen::program(&flat);
            println!("{}", gen);
            text.clear();
        }
        text.push_str(&line);
        text.push('\n');
    }
}

fn codegen_program(program: &str) {
    let parsed = parse_program(&program).unwrap();
    let flat = flatten::program(parsed);
    for func in &flat {
        println!("{:?}", func);
    }
    let gen = codegen::program(&flat);
    println!("{}", gen);
}

mod test {
    use super::*;

    #[test]
    fn trivial() {
        codegen_program(
            r#"
            fn main() -> Int = 3
        "#,
        );
    }

    #[test]
    fn monomorph() {
        codegen_program(
            r#"
            fn id[t](x: t) -> t = x
            fn main() -> Int = id[Int](3)
            "#,
        )
    }
}
