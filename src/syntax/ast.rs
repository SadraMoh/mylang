#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    // Print(Box<AstNode>),
    Int(i32),
    // DoublePrecisionFloat(f64),
    // MonadicOp {
    //     verb: MonadicVerb,
    //     expr: Box<AstNode>,
    // },
    // DyadicOp {
    //     verb: DyadicVerb,
    //     lhs: Box<AstNode>,
    //     rhs: Box<AstNode>,
    // },
    // Terms(Vec<AstNode>),
    VariableDeclaration {
        ident: String,
        init: Option<Box<AstNode>>,
    },
    Assignment {
        ident: String,
        value: Box<AstNode>,
    },
    Ident(String),
    Str(String),
}

#[derive(Debug)]
pub enum Expr {
    Int(i32),
    // BinOp {
    //     lhs: Box<Expr>,
    //     op: Op,
    //     rhs: Box<Expr>,
    // },
    // UnaryOp {
    //     op: Op,
    //     rhs: Box<Expr>,
    // },
}

#[derive(Debug)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}
