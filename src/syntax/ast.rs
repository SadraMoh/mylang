#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Print(Option<Box<AstNode>>),
    Int(i32),
    Ident(String),
    Str(String),
    BinOp {
        lhs: Box<AstNode>,
        op: BinOp,
        rhs: Box<AstNode>,
    },
    Conditional {
        cond: Box<AstNode>,
        body: Box<AstNode>,
        alt: Option<Box<AstNode>>,
    },
    Exprs(Vec<Box<AstNode>>),
    FunctionDeclaration {
        ident: String,
        params: Vec<String>,
        body: Vec<Box<AstNode>>,
    },
    ReturnExpression(Box<AstNode>),
    CallExpression {
        callee: String,
        args: Vec<Box<AstNode>>,
    },
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
}

#[derive(Debug)]
pub enum Expr {
    Int(i32),
    // UnaryOp {
    //     op: Op,
    //     rhs: Box<Expr>,
    // },
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOp {
    Or,
    And,
    Addition,
    Subtraction,
    Division,
    Multiplication,
    Power,
    Modulo,
    Is,
    Gt,
    Lt,
    Gte,
    Lte,
}

impl TryFrom<&str> for BinOp {
    type Error = String;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "or" => BinOp::Or,
            "and" => BinOp::And,
            "add" => BinOp::Addition,
            "sub" => BinOp::Subtraction,
            "div" => BinOp::Division,
            "mul" => BinOp::Multiplication,
            "pow" => BinOp::Power,
            "mod" => BinOp::Modulo,
            "is" => BinOp::Is,
            "gt" => BinOp::Gt,
            "lt" => BinOp::Lt,
            "gte" => BinOp::Gte,
            "lte" => BinOp::Lte,
            un => panic!("Unexpected string {un:?}"),
        })
    }
}
