use crate::data::{
    ExpressionLike,
    LineOrScope,
    Scope,
    Line,
    AST,
};

impl std::fmt::Display for ExpressionLike {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ExpressionLike::Empty => write!(f, "0"),
            ExpressionLike::Var(var) => write!(f, "{var}"),
            ExpressionLike::Val(num) => write!(f, "{num}"),
            ExpressionLike::Exp(node) => write!(f, "({} {} {})", node.left, node.op, node.right),
        }
    }
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Line::Statement(stmt) => write!(f, "{} = {}", stmt.var, stmt.exp),
            Line::Expression(exp) => write!(f, "{exp}"),
        }
    }
}

impl std::fmt::Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for line_or_scope in &self.inner {
            write!(f,  "{line_or_scope}\n")?;
        }
        writeln!(f, "}}")
    }
}

impl std::fmt::Display for LineOrScope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LineOrScope::Line(line) => write!(f, "{line}"),
            LineOrScope::Scope(scope) => write!(f, "{scope}"),
        }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let AST::Scope(scope) = self;
        write!(f, "{scope}")
    }
}

