use super::syntax::AstNode;

use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    CallExpr, Callee, Decl, Expr, ExprOrSpread, ExprStmt, Ident, Lit, MemberExpr, MemberProp,
    Number, Program, Script, Stmt, Str, VarDecl, VarDeclarator,
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
        _ => unimplemented!("to_js_expr not handled for Node: [{node:?}]"),
    }
}
