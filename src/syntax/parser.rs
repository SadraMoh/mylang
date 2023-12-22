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
            Rule::num => Expr::Integer(primary.as_str().parse::<i32>().unwrap()),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        // .map_infix(|lhs, op, rhs| {
        //     let op = match op.as_rule() {
        //         Rule::addition => crate::syntax::Op::Add,
        //         Rule::subtraction => crate::syntax::Op::Subtract,
        //         Rule::multiplication => crate::syntax::Op::Multiply,
        //         Rule::division => crate::syntax::Op::Divide,
        //         rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
        //     };
        //     Expr::BinOp {
        //         lhs: Box::new(lhs),
        //         op,
        //         rhs: Box::new(rhs),
        //     }
        // })
        .parse(pairs)
}

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        // Rule::monadicExpr => {
        //     let mut pair = pair.into_inner();
        //     let verb = pair.next().unwrap();
        //     let expr = pair.next().unwrap();
        //     let expr = build_ast_from_expr(expr);
        //     parse_monadic_verb(verb, expr)
        // }
        // Rule::dyadicExpr => {
        //     let mut pair = pair.into_inner();
        //     let lhspair = pair.next().unwrap();
        //     let lhs = build_ast_from_expr(lhspair);
        //     let verb = pair.next().unwrap();
        //     let rhspair = pair.next().unwrap();
        //     let rhs = build_ast_from_expr(rhspair);
        //     parse_dyadic_verb(verb, lhs, rhs)
        // }
        // Rule::terms => {
        //     let terms: Vec<AstNode> = pair.into_inner().map(build_ast_from_term).collect();
        //     // If there's just a single term, return it without
        //     // wrapping it in a Terms node.
        //     match terms.len() {
        //         1 => terms.get(0).unwrap().clone(),
        //         _ => Terms(terms),
        //     }
        // }
        Rule::assmnt => {
            let mut pair = pair.into_inner();
            let ident = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_expr(expr);
            AstNode::IsGlobal {
                ident: String::from(ident.as_str()),
                expr: Box::new(expr),
            }
        }
        Rule::var_declare => {
            print!("YOO HOO");
            panic!()
        }
        // Rule::string => {
        //     let str = &pair.as_str();
        //     // Strip leading and ending quotes.
        //     let str = &str[1..str.len() - 1];
        //     // Escaped string quotes become single quotes here.
        //     let str = str.replace("''", "'");
        //     AstNode::Str(CString::new(&str[..]).unwrap())
        // }
        unknown_expr => panic!("Unexpected expression: {:?}", unknown_expr),
    }
}

pub fn parse(source: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let mut ast = Vec::<AstNode>::new();

    let mut root = LangParser::parse(Rule::program, source).expect("Error parsing from root");
    let root = root.next().expect("Root to be available");

    let Rule::program = root.as_rule() else {
        panic!("Expected root to be of type _program")
    };

    for pair in root.into_inner() {
        match pair.as_rule() {
            Rule::expr => {
                ast.push(build_ast_from_expr(pair));
            }
            x => {
                println!("- {:?}", x)
            }
        }
    }

    Ok(ast)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = super::parse(
            "
              let hello
            ",
        );
        println!("result: {:?}", result);
    }
}
