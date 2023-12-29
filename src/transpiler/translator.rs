use crate::syntax::{AstNode, BinOp};
use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    ArrowExpr, AssignExpr, BinExpr, BinaryOp, BindingIdent, BlockStmt, BlockStmtOrExpr, CallExpr,
    Callee, Decl, Expr, ExprOrSpread, ExprStmt, FnDecl, Function, Ident, IfStmt, Lit, MemberExpr,
    MemberProp, Number, Param, ParenExpr, PatOrExpr, Program, ReturnStmt, Script, Stmt, Str,
    VarDecl, VarDeclarator,
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
        AstNode::ReturnExpression(val) => Stmt::Return(ReturnStmt {
            span: DUMMY_SP,
            arg: Some(Box::new(to_js_expr(*val))),
        }),
        AstNode::FunctionDeclaration {
            ident,
            params,
            body,
        } => {
            let params = params
                .into_iter()
                .map(|param| Param {
                    span: DUMMY_SP,
                    decorators: Vec::new(),
                    pat: swc_ecma_ast::Pat::Ident(BindingIdent {
                        type_ann: None,
                        id: Ident {
                            span: DUMMY_SP,
                            optional: false,
                            sym: param.into(),
                        },
                    }),
                })
                .collect();

            let body = body.into_iter().map(|expr| to_js_stmt(*expr)).collect();

            Stmt::Decl(Decl::Fn(FnDecl {
                declare: false,
                function: Box::new(Function {
                    span: DUMMY_SP,
                    decorators: Vec::new(),
                    is_async: false,
                    is_generator: false,
                    return_type: None,
                    type_params: None,
                    params,
                    body: Some(BlockStmt {
                        span: DUMMY_SP,
                        stmts: body,
                    }),
                }),
                ident: Ident {
                    span: DUMMY_SP,
                    optional: false,
                    sym: ident.into(),
                },
            }))
        }
        // Expression Statements
        AstNode::Int(_)
        | AstNode::Str(_)
        | AstNode::Ident(_)
        | AstNode::PipelineExpression(_)
        | AstNode::Assignment { ident: _, value: _ }
        | AstNode::CallExpression { callee: _, args: _ }
        | AstNode::Print(_) => Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: Box::new(to_js_expr(node)),
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
        AstNode::Print(expr) => {
            let args = vec![expr.and_then(|f| {
                Some(ExprOrSpread {
                    spread: None,
                    expr: Box::new(to_js_expr(*f)),
                })
            })];
            let args = args.into_iter().filter_map(|f| f).collect();

            Expr::Call(CallExpr {
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
            })
        }
        AstNode::Assignment { ident, value } => Expr::Assign(AssignExpr {
            span: DUMMY_SP,
            left: PatOrExpr::Expr(Box::new(Expr::Ident(Ident {
                span: DUMMY_SP,
                optional: false,
                sym: ident.into(),
            }))),
            op: swc_ecma_ast::AssignOp::Assign,
            right: Box::new(to_js_expr(*value)),
        }),
        AstNode::CallExpression { callee, args } => Expr::Call(CallExpr {
            span: DUMMY_SP,
            type_args: None,
            callee: Callee::Expr(Box::new(Expr::Ident(Ident {
                span: DUMMY_SP,
                optional: false,
                sym: callee.into(),
            }))),
            args: args
                .into_iter()
                .map(|arg| ExprOrSpread {
                    spread: None,
                    expr: Box::new(to_js_expr(*arg)),
                })
                .collect(),
        }),
        AstNode::PipelineExpression(steps) => {
            let mut stmts: Vec<Stmt> = Vec::with_capacity(steps.len() + 2);
            stmts.push(to_js_stmt(AstNode::VariableDeclaration {
                ident: "it".into(),
                init: None,
            }));
            steps.into_iter().for_each(|step| {
                stmts.push(to_js_stmt(AstNode::Assignment {
                    ident: "it".into(),
                    value: step,
                }))
            });
            stmts.push(to_js_stmt(AstNode::ReturnExpression(Box::new(
                AstNode::Ident("it".into()),
            ))));

            Expr::Call(CallExpr {
                span: DUMMY_SP,
                args: Vec::new(),
                type_args: None,
                callee: Callee::Expr(Box::new(Expr::Paren(ParenExpr {
                    span: DUMMY_SP,
                    expr: Box::new(Expr::Arrow(ArrowExpr {
                        span: DUMMY_SP,
                        params: Vec::new(),
                        is_async: false,
                        is_generator: false,
                        type_params: None,
                        return_type: None,
                        body: Box::new(BlockStmtOrExpr::BlockStmt(BlockStmt {
                            span: DUMMY_SP,
                            stmts,
                        })),
                    })),
                }))),
            })
        }

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
