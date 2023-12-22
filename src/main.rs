use std::{env, fs};

use mylang::syntax::{parse_expr, Rule};
use pest::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = args[1].clone();

    for line in fs::read_to_string(filename)
        .expect("Unable to read file")
        .lines()
    {
        match mylang::syntax::LangParser::parse(Rule::program, &line) {
            Ok(mut pairs) => {
                println!(
                    "Parsed: {:#?}",
                    parse_expr(
                        // inner of expr
                        pairs.next().unwrap().into_inner()
                    )
                );
            }
            Err(e) => {
                eprintln!("Parse failed: {:?}", e);
            }
        }
    }
}
