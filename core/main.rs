use chumsky::error::Simple;
use parser::parse_program;

use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    let mut text = String::new();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if line == "" {
            let parsed = match parse_program(&text) {
                Ok(p) => p,
                Err(errors) => {
                    for error in errors {
                        parse_error(&text, error);
                    }
                    return;
                }
            };
            let mut flat = flatten::program(parsed);
            println!("flattened program");
            let base = lambda_set::program(&mut flat);
            println!("lowered program");
            println!("{:?}", base);
            println!("printed program");
            /*
            let gen = codegen::program(&flat);
            println!("{}", gen);
            */
            text.clear();
        }
        text.push_str(&line);
        text.push('\n');
    }
}

fn parse_error(text: &str, error: Simple<char>) {
    use ariadne::*;
    Report::build(ReportKind::Error, error.span())
        .with_message(format!("Parse error"))
        .with_note(format!(
            "Expected one of {:?}",
            error.expected().collect::<Vec<_>>()
        ))
        .with_label(
            Label::new(error.span())
                .with_color(Color::Red)
                .with_message("Unexpected"),
        )
        .finish()
        .eprint(Source::from(text))
        .unwrap();
}

fn codegen_program(program: &str) {
    let parsed = parse_program(&program).unwrap();
    let mut flat = flatten::program(parsed);
    lambda_set::program(&mut flat);
    for func in &flat.functions {
        println!("{:?}", func);
    }
    let gen = codegen::program(&flat);
    println!("{}", gen);
}

#[cfg(test)]
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

    #[test]
    fn func_type() {
        codegen_program(
            r#"
            fn twice() -> (Int) -> Int = [](x: Int) -> Int = x
            fn const[a, b](x: a) -> (b) -> a = [x: a](y: b) -> a = x
            fn main() -> (Int) -> Int = const[Int, Int](3)
        "#,
        )
    }

    #[test]
    fn tuple_construction() {
        codegen_program(
            r#"
            fn main() -> <|Int, Int|> = <|3, 4|>
        "#,
        )
    }

    #[test]
    fn tuple_access() {
        codegen_program(
            r#"
            fn swap[a, b](tuple: <|a, b|>) -> <|b, a|> = <|tuple.1, tuple.0|>
            fn main() -> <|Int, Int|> = swap[Int, Int](<|3, 4|>)
        "#,
        );
    }

    #[test]
    fn enum_def() {
        codegen_program(
            r#"
            enum Result {
                ok(Int), err(Int)
            }
            fn main() -> Result = Result::ok(5)
        "#,
        )
    }

    #[test]
    fn match_enum() {
        codegen_program(
            r#"
            enum Result {
                ok(Int), err(Int)
            }
            fn decode(r: Result) -> <|Int, Int|> = match r {
                ok(o) => <|o, 0|>,
                err(e) => <|0, e|>
            }
            fn main() -> <|Int, Int|> = decode(Result::ok(3))
        "#,
        )
    }

    #[test]
    fn nested_match() {
        codegen_program(
            r#"
            enum A {
                b(Int), c(Int)
            }
            enum D {
                e(<|Int, Int|>),
                f(A)
            }
            fn decode_a(a: A) -> Int = match a {
                b(x) => x,
                c(x) => x
            }
            fn decode_d(d: D) -> Int = match d {
                e(pair) => pair.0,
                f(a) => decode_a(a)
            }
        "#,
        )
    }

    #[test]
    fn direct_match() {
        codegen_program(
            r#"
            enum Box {
                box(Int)
            }

            fn three() -> Int = match Box::box(3) {
                box(x) => x
            }
        "#,
        )
    }
}
