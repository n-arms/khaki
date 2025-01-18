#![deny(clippy::all)]

use chumsky::error::Simple;
use flatten::flatten_program;
use ir::token::Token;
use lambda_set::defunctionalize_program;
use parser::parse_program;
use typer::type_program;

use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    let mut text = String::new();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if line.is_empty() {
            let parse_env = parser::Env::new(text.clone());
            let parsed = match parse_program(&parse_env) {
                Ok(p) => p,
                Err(parser::Error::LexingFailed) => {
                    return;
                }
                Err(parser::Error::ParseError(errors)) => {
                    for error in errors {
                        parse_error(&text, error);
                    }
                    return;
                }
            };
            println!("parsed: {:?}", parsed);
            let typed = match type_program(parsed, parse_env.lamda_sets()) {
                Ok(t) => t,
                Err(error) => {
                    type_error(&text, error);
                    return;
                }
            };

            println!("typed: {:?}", typed);

            let flat = flatten_program(&typed);

            println!("flat: {:?}", flat);
            /*
            let mut flat = flatten::program(parsed);
            let base = lambda_set::program(&mut flat);
            println!("{:?}", base);

            let c = gen_program(&base).generate();

            //println!("{}", c);

            fs::write("./target/test.c", c).unwrap();
            */
            text.clear();
        }
        text.push_str(&line);
        text.push('\n');
    }
}

fn type_error(_text: &str, error: typer::Error) {
    panic!("{:?}", error)
}

fn parse_error(text: &str, error: Simple<Token>) {
    use ariadne::*;
    Report::build(ReportKind::Error, error.span())
        .with_message("Parse error")
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

#[cfg(test)]
mod test {
    use std::{fs, path::Path, process::Command};

    use super::*;

    fn run_program(program: &str, working: impl AsRef<Path> + Clone) -> i32 {
        let parse_env = parser::Env::new(program.to_string());
        let parsed = parse_program(&parse_env).unwrap();
        /*
        let mut flat = flatten::program(parsed);
        let base = lambda_set::program(&mut flat);
        let data = codegen::gen_program(&base).generate();
        println!("{}", data);
        let name = format!("test{}", program.len());
        let mut source = working.as_ref().to_path_buf();
        source.push(name.clone());
        source.set_extension("c");
        let mut binary = working.as_ref().to_path_buf();
        binary.push(name.clone());
        fs::write(source.clone(), data).unwrap();
        assert!(Command::new("gcc")
            .arg("-o")
            .arg(binary.as_os_str())
            .arg(source.as_os_str())
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .success());
        let output = Command::new(binary.as_os_str())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap();
        output.status.code().unwrap()
        */
        0
    }

    macro_rules! test_program {
        ($program:expr, $result:expr) => {{
            let run_result = run_program($program, "./target/");
            if run_result != $result {
                panic!(
                    "Test failed: program output {} != expected {}\n\n{}",
                    run_result, $result, $program
                )
            }
        }};
    }

    #[test]
    fn trivial() {
        test_program!(
            r#"
                fn main() -> Int = 2
            "#,
            2
        );
    }

    #[test]
    fn tuple() {
        test_program!(
            r#"
                fn main() -> Int = <|4, 3|>.1
            "#,
            3
        )
    }

    #[test]
    fn monomorph() {
        test_program!(
            r#"
                fn id[t](x: t) -> t = x
                fn main() -> Int = id[Int](4)
            "#,
            4
        );
    }

    #[test]
    fn func_type() {
        test_program!(
            r#"
                fn twice() -> (Int) -> Int = [](x: Int) -> Int = x
                fn const[a, b](x: a) -> (b) -> a = [x: a](y: b) -> a = x
                fn main() -> Int = const[Int, Int](5)(6)
            "#,
            5
        );
    }

    #[test]
    fn calling_closures() {
        test_program!(
            r#"
                fn main() -> Int = ([](x: Int) -> Int = x)(7)
            "#,
            7
        );
    }

    #[test]
    fn calling_higher_order() {
        test_program!(
            r#"
                fn twice() -> () -> Int = []() -> Int = 8
                fn main() -> Int = twice()()
            "#,
            8
        );
    }

    #[test]
    fn captures() {
        test_program!(
            r#"
                fn f(x: Int) -> Int = ([x: Int](y: Int) -> Int = x)(5)
                fn main() -> Int = f(9)
            "#,
            9
        );
    }

    #[test]
    fn tuple_access() {
        test_program!(
            r#"
                fn swap[a, b](tuple: <|a, b|>) -> <|b, a|> = <|tuple.1, tuple.0|>
                fn main() -> Int = swap[Int, Int](<|10, 11|>).0
            "#,
            11
        );
    }

    #[test]
    fn match_enum() {
        test_program!(
            r#"
                enum Result {
                    ok(Int), err(Int)
                }
                fn decode(r: Result) -> <|Int, Int|> = match r {
                    ok(o) => <|o, 0|>,
                    err(e) => <|0, e|>
                }
                fn main() -> Int = decode(Result::ok(12)).0
            "#,
            12
        );
    }

    #[test]
    fn nested_match() {
        test_program!(
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
                fn main() -> Int = decode_d(D::f(A::b(13)))
            "#,
            13
        );
    }

    #[test]
    fn direct_match() {
        test_program!(
            r#"
                enum Box {
                    box(Int)
                }

                fn main() -> Int = match Box::box(14) {
                    box(x) => x
                }
            "#,
            14
        );
    }

    #[test]
    fn different_named_arguments() {
        test_program!(
            r#"
                enum Bool {
                    true(<||>),
                    false(<||>)
                }
                fn main() -> Int = (match Bool::true(<||>) {
                    true(x) => [](a: Int) -> Int = a,
                    false(x) => [](b: Int) -> Int = b
                })(15)
            "#,
            15
        )
    }

    #[test]
    fn lambdas_in_enums() {
        test_program!(
            r#"
                enum Thunk {
                    f(() -> Int)
                }
                fn main() -> Int = match <|Thunk::f([]() -> Int = 16), Thunk::f([]() -> Int = 17)|>.0 {
                    f(x) => x()
                }       
            "#,
            16
        )
    }
}
