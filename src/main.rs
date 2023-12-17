use std::{env, fs};

use lazy_static::lazy_static;
use mylang::syntax::Expr;
use pest::{iterators::Pairs, pratt_parser::PrattParser, Parser};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "syntax/.pest"]
struct Calculator;

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            // Addition and subtract have equal precedence
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
    };
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::integer => Expr::Integer(primary.as_str().parse::<i32>().unwrap()),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => mylang::syntax::Op::Add,
                Rule::subtract => mylang::syntax::Op::Subtract,
                Rule::multiply => mylang::syntax::Op::Multiply,
                Rule::divide => mylang::syntax::Op::Divide,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = args[1].clone();

    for line in fs::read_to_string(filename)
        .expect("Unable to read file")
        .lines()
    {
        match Calculator::parse(Rule::equation, &line) {
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
