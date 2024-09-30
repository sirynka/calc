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
    let write_keyword_template = |f: &mut std::fmt::Formatter, keyword: &str, expr: &ExpressionLike, scope: &Scope| -> std::fmt::Result {
        let spaces = SPACES.repeat(indent);
        write!(f, "{spaces}{keyword} ({expr}) {{\n")?;
        write_scope(f, scope, indent + 1)?;
        writeln!(f, "{spaces}}}")
    };

    match keyword {
        Keyword::Repeat(repeat) => write_keyword_template(f, "repeat", &repeat.count, &repeat.scope),
        Keyword::While(whhile) => write_keyword_template(f, "while", &whhile.condition, &whhile.scope),
        Keyword::If(iff) => write_keyword_template(f, "if", &iff.condition, &iff.scope),
    }
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
