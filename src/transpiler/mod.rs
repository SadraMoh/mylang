use super::syntax::AstNode;

use swc_common::DUMMY_SP;
use swc_ecma_ast::Decl;
use swc_ecma_ast::Expr;
use swc_ecma_ast::Ident;
use swc_ecma_ast::Lit;
use swc_ecma_ast::Number;
use swc_ecma_ast::Program;
use swc_ecma_ast::Script;
use swc_ecma_ast::Stmt;
use swc_ecma_ast::Str;
use swc_ecma_ast::VarDecl;
use swc_ecma_ast::VarDeclarator;

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
        _ => unimplemented!("to_js_stmt not handled for Node: [{node:?}]"),
    }
}

pub fn to_js_expr(node: AstNode) -> Expr {
    match node {
        AstNode::Int(val) => Expr::Lit(Lit::Num(Number {
            raw: None,
            span: DUMMY_SP,
            value: val as f64,
        })),
        AstNode::Str(val) => Expr::Lit(Lit::Str(Str {
            raw: None,
            span: DUMMY_SP,
            value: val.into(),
        })),
        _ => unimplemented!("to_js_expr not handled for Node: [{node:?}]"),
    }
}
