pub type Stack<'a> = std::collections::HashMap<String, Literal>;

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
    Repeat,
    While,
    If,
    Dot,
    Stack,
}

pub enum AST {
    Scope(Scope)
}

#[derive(Debug)]
pub enum EolSeparated {
    Line(Line),
    Scope(Scope),
    Keyword(Keyword),
}

#[derive(Debug)]
pub enum Line {
    Statement(Statement),
    Expression(ExpressionLike),
}

#[derive(Debug)]
pub enum Keyword {
    Repeat(Repeat),
    While(While),
    If(If),
    Stack,
}

#[derive(Debug, PartialEq)]
pub enum ExpressionLike {
    Var(VarName),
    Val(Literal),
    Exp(Box<BinaryOp>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    I64(i64),
    F64(f64),
}

#[derive(Debug, PartialEq)]
pub struct BinaryOp {
    pub op: String,
    pub left: ExpressionLike,
    pub right: ExpressionLike,
}

#[derive(Debug)]
pub struct Statement {
    pub var: VarName,
    pub exp: ExpressionLike,
}

#[derive(Debug, PartialEq)]
pub enum VarName {
    Simple(String),
    Indexable(String, Box<ExpressionLike>)
}

#[derive(Debug)]
pub struct Repeat {
    pub count: ExpressionLike,
    pub scope: Scope,
}

#[derive(Debug)]
pub struct If {
    pub condition: ExpressionLike,
    pub scope: Scope,
}

#[derive(Debug)]
pub struct While {
    pub condition: ExpressionLike,
    pub scope: Scope,
}

#[derive(Debug)]
pub struct Scope {
    pub inner: Vec<EolSeparated>,
}

