pub type Stack<'a> = std::collections::HashMap<String, i64>;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Literal(String),
    Variable(String),
    Op(String),
    OpenParen,
    ClosedParen,
    OpenCurly,
    ClosedCurly,
    Equal,
    EndOfLine,
}

pub enum AST {
    Scope(Scope)
}

#[derive(Debug)]
pub enum LineOrScope {
    Line(Line),
    Scope(Scope),
}

#[derive(Debug)]
pub enum Line {
    Statement(Statement),
    Expression(ExpressionLike),
}

#[derive(Debug, PartialEq)]
pub enum ExpressionLike {
    Var(String),
    Val(String),
    Exp(Box<BinaryOp>),
    Empty,
}

#[derive(Debug, PartialEq)]
pub struct BinaryOp {
    pub op: String,
    pub left: ExpressionLike,
    pub right: ExpressionLike,
}

#[derive(Debug)]
pub struct Statement {
    pub var: String,
    pub exp: ExpressionLike,
}

#[derive(Debug)]
pub struct Scope {
    pub inner: Vec<LineOrScope>,
}

