use flatten;
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
            for func in flat {
                println!("{:?}", func);
            }
            text.clear();
        }
        text.push_str(&line);
        text.push('\n');
    }
}
