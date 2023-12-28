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
        Rule::print_expr => {
            let mut pairs = pair.into_inner();
            let _print = pairs.next().expect("Expected 'print' in an [print_expr:0]");
            let expr = pairs
                .next()
                .and_then(|ex| Some(Box::new(build_ast_from_expr(ex))));

            AstNode::Print(expr)
        }
        Rule::assmnt => {
            let mut pairs = pair.into_inner();
            let ident = pairs.next().expect("Expected [ident] in an [assmnt:0]");
            let _load = pairs.next().expect("Expected 'load' in an [assmnt:1]");
            let expr = pairs.next().expect("Expected [expr] in an [assmnt:2]");
            let expr = build_ast_from_expr(expr);

            AstNode::Assignment {
                ident: ident.as_str().into(),
                value: Box::new(expr),
            }
        }
        Rule::var_declare => {
            let mut pairs = pair.into_inner();
            let _let = pairs.next().expect("Expected 'let' in a [var_declare:0]");
            let ident = pairs
                .next()
                .expect("Expected an '[ident]' in a [var_declare:1]");

            let init = pairs.next().and_then(|next| match next.as_rule() {
                Rule::var_init => Some(Box::new(build_ast_from_expr(next))),
                other => panic!("Expected 'None' or [var_init] at [var_declare:2] found {other:?}"),
            });

            AstNode::VariableDeclaration {
                ident: ident.as_str().into(),
                init: init,
            }
        }
        Rule::var_init => {
            let mut pairs = pair.into_inner();

            let _load = pairs.next().expect("Expected 'load' in a [var_init:0]");
            let expr = pairs
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
        Rule::if_expr | Rule::elif_seg => {
            let mut pairs = pair.into_inner();
            let _if = pairs.next().expect("Expected 'if' in [if_expr|elif_seg:0]");
            let cond_expr = pairs
                .next()
                .expect("Expected condition as [expr] in [if_expr|elif_seg:1]");
            let _do = pairs.next().expect("Expected 'do' in [if_expr|elif_seg:2]");

            let body_expr = pairs
                .next()
                .expect("Expected [expr] inf [if_expr|elif_seg:3]");

            let alt = pairs.next().and_then(|inner| match inner.as_rule() {
                Rule::end => None,
                _ => Some(Box::new(build_ast_from_expr(inner))),
            });

            AstNode::Conditional {
                cond: Box::new(build_ast_from_expr(cond_expr)),
                body: Box::new(build_ast_from_expr(body_expr)),
                alt: alt,
            }
        }
        Rule::else_seg => {
            let mut pairs = pair.into_inner();
            let _else = pairs.next().expect("Expected 'else' in [else_seg:0]");
            let _do = pairs.next().expect("Expected 'do' in [else_seg:1]");

            let body_expr = pairs.next().expect("Expected [expr] in [else_seg:2]");

            build_ast_from_expr(body_expr)
        }
        Rule::exprs => {
            let pairs = pair.into_inner();

            let mut res = Vec::new();
            for expr in pairs.into_iter() {
                res.push(Box::new(build_ast_from_expr(expr)));
            }

            AstNode::Exprs(res)
        }
        Rule::binary_expr => {
            let mut pairs = pair.into_inner();
            let logical_operand = pairs
                .next()
                .expect("Expected [logical_operand] in [binary_expr:0]");
            let binary_op = pairs
                .next()
                .expect("Expected [binary_op] in [binary_expr:1]");
            let expr = pairs.next().expect("Expected [expr] in [binary_expr:2]");

            AstNode::BinOp {
                lhs: Box::new(build_ast_from_expr(logical_operand)),
                op: binary_op.as_str().try_into().unwrap(),
                rhs: Box::new(build_ast_from_expr(expr)),
            }
        }
        Rule::logical_operand => build_ast_from_expr(
            pair.into_inner()
                .next()
                .expect("Expected logical [_] in [logical_operand:0]"),
        ),
        Rule::func => {
            let mut pairs = pair.into_inner();
            let _fun = pairs.next().expect("Expected 'fun' at [func:0]");
            let func_ident = pairs.next().expect("Expected [ident] at [func:1]");
            let func_params = pairs.next().expect("Expected [func_params] at [func:2]");

            let param = func_params.into_inner();
            let params: Vec<_> = param.into_iter().map(|p| p.as_str().to_string()).collect();

            let _do = pairs.next().expect("Expected 'do' at [func:3]");

            let func_body = pairs.next().expect("Expected [func_body] at [func:4]");
            let body: Vec<_> = func_body
                .into_inner()
                .into_iter()
                .map(|p| Box::new(build_ast_from_expr(p)))
                .collect();

            let _end = pairs.next().expect("Expected 'end' at [func:5]");

            AstNode::FunctionDeclaration {
                ident: func_ident.as_str().into(),
                params: params,
                body: body,
            }
        }
        Rule::func_return => {
            let mut pairs = pair.into_inner();
            let _ret = pairs.next().expect("Expected 'ret' at [func_return:0]");
            let expr = pairs.next().expect("Expected [expr] at [func_return:1]");

            AstNode::ReturnExpression(Box::new(build_ast_from_expr(expr)))
        }
        Rule::idents => {
            let pairs = pair.into_inner();
            let elements: Vec<_> = pairs.into_iter().collect();

            match elements.as_slice() {
                [] => panic!("Expected at least one member in an [idents]"),
                [ident] => {
                    // single
                    AstNode::Ident(ident.as_str().into())
                }
                [ident, args @ ..] => {
                    // multiple
                    AstNode::CallExpression {
                        callee: ident.as_str().into(),
                        args: args
                            .into_iter()
                            .map(|f| Box::new(build_ast_from_expr(f.clone())))
                            .collect(),
                    }
                }
            }
        }
        Rule::ident => AstNode::Ident(pair.as_str().into()),
        Rule::pipeline => {
            let pairs = pair.into_inner();
            let pairs: Vec<_> = pairs.collect();

            match pairs.as_slice() {
                [] => {
                    panic!("Expected [pipeline] to contain at least one [pipable_expr] and one [pipeline_seg")
                }
                [_] => {
                    panic!("Expected [pipeline] to contain at least one [pipeline_seg]")
                }
                [start, segments @ ..] => {
                    let mut steps = Vec::with_capacity(1 + segments.len());
                    steps.push(Box::new(build_ast_from_expr(start.clone())));
                    segments.iter().for_each(|seg| {
                        steps.push(Box::new(build_ast_from_expr(seg.clone())));
                    });

                    AstNode::PipelineExpression(steps)
                }
            }
        }
        Rule::pipeline_seg => {
            let mut pairs = pair.into_inner();
            let _pipe = pairs.next().expect("Expected 'pipe' at [pipeline_seg:0]");
            let expr = pairs
                .next()
                .expect("Expected [pipable_expr] at [pipable_expr:1]");

            build_ast_from_expr(expr)
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

    #[test]
    fn print() {
        let result = super::parse(
            "
                print

                print 'hello world'
            ",
        );
        println!("result: {:?}", result);
    }

    #[test]
    fn if_expression() {
        let result = super::parse(
            "
                if cond is 'hello' do
                    print 'yes'
                else do
                    print 'no'
                end
            ",
        );
        println!("result: {:?}", result);
    }

    #[test]
    fn function_declaration() {
        let result = super::parse(
            "
                fun addition x y do
                    ret x add y
                end
            ",
        );
        println!("result: {:?}", result);
    }

    #[test]
    fn function_call() {
        let result = super::parse(
            "
                addition x y

                addition x y z

                hello 'world' boop
            ",
        );
        println!("result: {:?}", result);
    }

    #[test]
    fn pipeline() {
        let result = super::parse(
            "
                15
                    pipe addOne it
                    pipe addFive it 
                    pipe addition 'love you ' it
                    pipe addition it ' times'
            ",
        );
        println!("result: {:?}", result);
    }
}
