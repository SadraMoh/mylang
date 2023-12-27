use crate::syntax::BinOp;

use super::syntax::AstNode;

use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    BinExpr, BinaryOp, BlockStmt, CallExpr, Callee, Decl, Expr, ExprOrSpread, ExprStmt, Ident,
    IfStmt, Lit, MemberExpr, MemberProp, Number, Program, Script, Stmt, Str, VarDecl,
    VarDeclarator,
};

pub fn to_js_program(root: Vec<AstNode>) -> Program {
    let body: Vec<Stmt> = root.into_iter().map(to_js_stmt).collect::<Vec<_>>();

    let script = Script {
        span: DUMMY_SP,
        shebang: None,
        body: body,
    };

    Program::Script(script)
}

pub fn to_js_stmt(node: AstNode) -> Stmt {
    match node {
        AstNode::VariableDeclaration { ident, init } => {
            let init: Option<Box<Expr>> = init.and_then(|some| Some(Box::new(to_js_expr(*some))));

            Stmt::Decl(Decl::Var(Box::new(VarDecl {
                declare: false,
                kind: swc_ecma_ast::VarDeclKind::Let,
                span: DUMMY_SP,
                decls: vec![VarDeclarator {
                    definite: false,
                    span: DUMMY_SP,
                    name: Ident {
                        span: DUMMY_SP,
                        sym: ident.into(),
                        optional: false,
                    }
                    .into(),
                    init: init,
                }],
            })))
        }
        AstNode::Print(expr) => {
            let args = vec![expr.and_then(|f| {
                Some(ExprOrSpread {
                    spread: None,
                    expr: Box::new(to_js_expr(*f)),
                })
            })];
            let args = args.into_iter().filter_map(|f| f).collect();

            Stmt::Expr(ExprStmt {
                span: DUMMY_SP,
                expr: Box::new(Expr::Call(CallExpr {
                    span: DUMMY_SP,
                    type_args: None,
                    args: args,
                    callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                        span: DUMMY_SP,
                        obj: Box::new(Expr::Ident(Ident {
                            span: DUMMY_SP,
                            optional: false,
                            sym: "console".into(),
                        })),
                        prop: MemberProp::Ident(Ident {
                            span: DUMMY_SP,
                            optional: false,
                            sym: "log".into(),
                        }),
                    }))),
                })),
            })
        }
        AstNode::Conditional { cond, body, alt } => Stmt::If(IfStmt {
            span: DUMMY_SP,
            test: Box::new(to_js_expr(*cond)),
            alt: alt.and_then(|inner| Some(Box::new(to_js_stmt(*inner)))),
            cons: Box::new(to_js_stmt(*body)),
        }),
        AstNode::Exprs(exprs) => Stmt::Block(BlockStmt {
            span: DUMMY_SP,
            stmts: exprs.into_iter().map(|f| to_js_stmt(*f)).collect(),
        }),
        _ => unimplemented!("to_js_stmt not handled for Node: [{node:?}]"),
    }
}

pub fn to_js_expr(node: AstNode) -> Expr {
    match node {
        AstNode::Int(val) => Expr::Lit(Lit::Num(Number {
            span: DUMMY_SP,
            raw: None,
            value: val as f64,
        })),
        AstNode::Str(val) => Expr::Lit(Lit::Str(Str {
            span: DUMMY_SP,
            raw: None,
            value: val.into(),
        })),
        AstNode::Ident(val) => Expr::Ident(Ident {
            span: DUMMY_SP,
            optional: false,
            sym: val.into(),
        }),
        AstNode::BinOp { lhs, op, rhs } => Expr::Bin(BinExpr {
            span: DUMMY_SP,
            left: Box::new(to_js_expr(*lhs)),
            op: to_js_bin_op(op),
            right: Box::new(to_js_expr(*rhs)),
        }),
        _ => unimplemented!("to_js_expr not handled for Node: [{node:?}]"),
    }
}

pub fn to_js_bin_op(op: BinOp) -> BinaryOp {
    match op {
        BinOp::Or => BinaryOp::LogicalOr,
        BinOp::And => BinaryOp::LogicalAnd,
        BinOp::Addition => BinaryOp::Add,
        BinOp::Subtraction => BinaryOp::Sub,
        BinOp::Division => BinaryOp::Div,
        BinOp::Multiplication => BinaryOp::Mul,
        BinOp::Power => BinaryOp::Exp,
        BinOp::Modulo => BinaryOp::Mod,
        BinOp::Is => BinaryOp::EqEqEq,
        BinOp::Gt => BinaryOp::Gt,
        BinOp::Lt => BinaryOp::Lt,
        BinOp::Gte => BinaryOp::GtEq,
        BinOp::Lte => BinaryOp::LtEq,
    }
}
