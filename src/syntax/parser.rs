use lazy_static::lazy_static;
use pest::{error::Error, iterators::Pairs, pratt_parser::PrattParser, Parser};
use pest_derive::Parser;

use super::{AstNode, Expr};

#[derive(Parser)]
#[grammar = "syntax/.pest"]
pub struct LangParser;

lazy_static! {
    pub static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            // Addition and subtract have equal precedence
            .op(Op::infix(addition, Left) | Op::infix(subtraction, Left))
            .op(Op::infix(multiplication, Left) | Op::infix(division, Left))
            .op(Op::infix(power, Left))
    };
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::num => Expr::Int(primary.as_str().parse::<i32>().unwrap()),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        .parse(pairs)
}

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::assmnt => {
            let mut pair = pair.into_inner();
            let ident = pair.next().expect("Expected [ident] in an [assmnt:0]");
            let _load = pair.next().expect("Expected 'load' in an [assmnt:1]");
            let expr = pair.next().expect("Expected [expr] in an [assmnt:2]");
            let expr = build_ast_from_expr(expr);

            AstNode::Assignment {
                ident: ident.as_str().into(),
                value: Box::new(expr),
            }
        }
        Rule::var_declare => {
            let mut pair = pair.into_inner();
            let _let = pair.next().expect("Expected 'let' in a [var_declare:0]");
            let ident = pair
                .next()
                .expect("Expected an '[ident]' in a [var_declare:1]");

            let init = pair.next().and_then(|next| match next.as_rule() {
                Rule::var_init => Some(Box::new(build_ast_from_expr(next))),
                other => panic!("Expected 'None' or [var_init] at [var_declare:2] found {other:?}"),
            });

            AstNode::VariableDeclaration {
                ident: ident.as_str().into(),
                init: init,
            }
        }
        Rule::var_init => {
            let mut pair = pair.into_inner();

            let _load = pair.next().expect("Expected 'load' in a [var_init:0]");
            let expr = pair
                .next()
                .expect("Expected an '[expr]' in a [var_declare:1]");

            build_ast_from_expr(expr)
        }
        Rule::str => {
            let str = &pair.as_str();
            // Strip leading and ending quotes.
            let str = &str[1..str.len() - 1];
            AstNode::Str(String::from(str))
        }
        Rule::int => {
            let int = pair.as_str();
            let int = int
                .parse()
                .expect(format!("Unable to parse {} to int", int).as_str());
            AstNode::Int(int)
        }
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = Vec::<AstNode>::new();

    let mut root = LangParser::parse(Rule::program, source).expect("Error parsing from root");
    let root = root.next().expect("Root to be available");

    let Rule::program = root.as_rule() else {
        panic!("Expected root to be of type [program]")
    };

    let root = root
        .into_inner()
        .next()
        .expect("[program] to have children");
    let Rule::exprs = root.as_rule() else {
        panic!("Expected the first child of [program] to be of type [exprs]")
    };

    for pair in root.into_inner() {
        match pair.as_rule() {
            Rule::expr => {
                ast.push(build_ast_from_expr(pair));
            }
            x => {
                println!("Ignored rule in exprs {x:?}")
            }
        }
    }

    Ok(ast)
}

#[cfg(test)]
mod tests {
    #[test]
    fn vars() {
        let result = super::parse(
            "
                let a

                let b load 1
                
                b load 'hello'
            ",
        );
        println!("result: {:?}", result);
    }
}
