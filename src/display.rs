use crate::data::{
    ExpressionLike,
    EolSeparated,
    Keyword,
    Scope,
    Line,
    AST,
};

const SPACES: &str = "    ";
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

fn write_keyword(f: &mut std::fmt::Formatter, keyword: &Keyword, indent: usize) -> std::fmt::Result {
    let spaces = SPACES.repeat(indent);
    match keyword {
        Keyword::Repeat(repeat) => {
            writeln!(f, "{spaces}repeat({}) {{", repeat.count)?;
            write_scope(f, &repeat.scope, indent + 1)?;
            writeln!(f, "{spaces}}}")?;
        }
    }
    Ok(())
}

fn write_scope(f: &mut std::fmt::Formatter, scope: &Scope, indent: usize) -> std::fmt::Result {
    let spaces = SPACES.repeat(indent);
    for inner in &scope.inner {
        match inner {
            EolSeparated::Line(line) => writeln!(f, "{spaces}{line}")?,
            EolSeparated::Scope(scope) => write_scope(f, scope, indent + 1)?,
            EolSeparated::Keyword(keyword) => write_keyword(f, keyword, indent)?,
        }
    }
    Ok(())
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let AST::Scope(scope) = self;
        write_scope(f, scope, 0)
    }
}
