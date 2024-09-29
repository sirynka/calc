type Stack<'a> = std::collections::HashMap<&'a str, i64>;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Literal(String),
    Variable(String),
    Op(String),
    OpenParen,
    ClosedParen,
    Equal,
    EndOfLine,
}

fn tokenize(s: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.peek() {
        match c {
            'a'..='z' => {
                let mut token = String::new();
                while let Some(c) = chars.next_if(|c| c.is_alphabetic() || c.is_digit(10)) { token.push(c); }
                tokens.push(Token::Variable(token));
            }
            '0'..='9' => {
                let mut token = String::new();
                while let Some(c) = chars.next_if(|c| c.is_digit(10)) { token.push(c); }
                tokens.push(Token::Literal(token));
            }
            '+' | '-' | '/' | '%' => { if let Some(c) = chars.next() { tokens.push(Token::Op(c.to_string())); } }
            '(' => { if let Some(_) = chars.next() { tokens.push(Token::OpenParen) } }
            ')' => { if let Some(_) = chars.next() { tokens.push(Token::ClosedParen) } }
            '=' => { if let Some(_) = chars.next() { tokens.push(Token::Equal) } }
            '\n' | ';' => { if let Some(_) = chars.next() { tokens.push(Token::EndOfLine) } }
            '*' => {
                chars.next();
                let star = chars.peek() == Some(&'*');
                let stars = if star { "**" } else { "*" };
                if star { chars.next(); }
                tokens.push(Token::Op(String::from(stars)));
            }
            _ => {
                chars.next();
            }
        }
    }

    if tokens.first() != Some(&Token::EndOfLine) {
        tokens.insert(0, Token::EndOfLine);
    }

    if tokens.last() != Some(&Token::EndOfLine) {
        tokens.push(Token::EndOfLine);
    }

    tokens
}

enum AST {
    Lines(Vec<Line>),
}

enum Line {
    Statement(Statement),
    Expression(ExpressionLike),
}

#[derive(Debug, PartialEq)]
enum ExpressionLike {
    Var(String),
    Val(String),
    Exp(Box<BinaryOp>),
    Empty,
}

#[derive(Debug, PartialEq)]
struct BinaryOp {
    op: String,
    left: ExpressionLike,
    right: ExpressionLike,
}

struct Statement {
    var: String,
    exp: ExpressionLike,
}

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

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AST::Lines(lines) => {
                let lines = lines.iter().enumerate().map(|(i, line)| (line, i == 0));
                for (line, first) in lines {
                    if !first { writeln!(f)?; }
                    match line {
                        Line::Statement(stmt) => write!(f, "{} = {}", stmt.var, stmt.exp)?,
                        Line::Expression(exp) => write!(f, "{}", exp)?,
                    };
                }
                Ok(())
            }
        }
    }
}

fn parse_exp(tokens: &[Token]) -> ExpressionLike {
    let op_precedence = ["-", "+", "*", "/", "%", "**"];

    let wrapped_in_parens = |tokens: &[Token]| -> bool {
        tokens.len() > 2 && tokens[0] == Token::OpenParen && tokens[tokens.len() - 1] == Token::ClosedParen
    };

    fn strip_parens(tokens: &[Token]) -> &[Token] {
        let mut tokens = tokens;
        while let (Some(_), Some(_)) = (tokens.strip_prefix(&[Token::OpenParen]), tokens.strip_suffix(&[Token::ClosedParen])) {
            tokens = &tokens[1..tokens.len() - 1];
        }
        tokens
    }

    let parse_tokens_as_exp = |op_idx: usize| {
        let Token::Op(op) = tokens[op_idx].clone() else { unreachable!(); };

        ExpressionLike::Exp(
            Box::new(
                BinaryOp {
                    op,
                    left: parse_exp(&tokens[..op_idx]),
                    right: parse_exp(&tokens[op_idx + 1..]),
                }
            )
        )
    };

    let parse_tokens_as_val = || {
        match tokens.len() {
            0 => return ExpressionLike::Empty,
            1 => return {
                match &tokens[0] {
                    Token::Literal(num) => ExpressionLike::Val(num.clone()),
                    Token::Variable(var) => ExpressionLike::Var(var.clone()),
                    _ => unreachable!(),
                }
            },
            _ => {
                if wrapped_in_parens(tokens) { return parse_exp(strip_parens(tokens)); }
                panic!("\n{}{}{}{}\n",
                    "Failed to parse tokens into the ast.\n",
                    "The list of tokens does not contain any operators,\n",
                    "those it should only contain a single token, tokens wrapped in parens or noting\n",
                    format!("Found {tokens:?}")
                );
            },
        }
    };

    let find_posible_splits = |tokens: &[Token]| -> Vec<usize> {
        let mut count = 0;
        let mut splits = Vec::new();
        for (i, token) in tokens.iter().enumerate() {
            match token {
                Token::OpenParen => count += 1,
                Token::ClosedParen => count -= 1,
                Token::Op(_) => if count == 0 { splits.push(i); },
                _ => {}
            }
        }
        splits
    };

    let pick_split = |splits: &[usize]| -> Option<usize> {
        splits.iter()
            .filter_map(|&i| if let Token::Op(op) = &tokens[i] { Some((i, op)) } else { None })
            .min_by_key(|&(_, op)| op_precedence.iter().position(|&p| p == op))
            .map(|(i, _)| i)
    };

    let possible_splits = find_posible_splits(tokens);
    let split_pos = pick_split(&possible_splits);
    match split_pos {
        Some(op_idx) => parse_tokens_as_exp(op_idx),
        None => parse_tokens_as_val(),
    }
}

fn parse_stmt(tokens: &[Token]) -> Statement {
    if tokens[1] != Token::Equal {
        panic!("{}\n{}",
            "Statements can not contain anything but '=' as tok[1]",
            format!("Found {tokens:?}")
        );
    }

    let Token::Variable(var) = &tokens[0] else {
        panic!("Cannot assign {:?} to {:?}", tokens[0], &tokens[2..]);
    };

    Statement {
        var: var.clone(),
        exp: parse_exp(&tokens[2..]),
    }
}

fn parse_lines(tokens: &[Token]) -> AST {
    let mut eol_pos: Vec<usize> = Vec::new();
    for (i, token) in tokens.iter().enumerate() {
        if let Token::EndOfLine = token { eol_pos.push(i); }
    }

    let mut lines: Vec<Line> = Vec::new();
    for w in eol_pos.windows(2) {
        if let [b, e] = w {
            let line = &tokens[*b+1..*e];
            if line.is_empty() { continue; }
            match line.contains(&Token::Equal) {
                true => lines.push(Line::Statement(parse_stmt(line))),
                false => lines.push(Line::Expression(parse_exp(line))),
            }
        }
    }

    AST::Lines(lines)
}

fn pow(base: i64, exp: i64) -> i64 {
    let mut res = 1;
    for _ in 0..exp { res *= base; }
    res
}

fn eval(node: &ExpressionLike, stack: &Stack) -> i64 {
    match node {
        ExpressionLike::Empty => 0,
        ExpressionLike::Val(num) => num.parse().unwrap(),
        ExpressionLike::Exp(node) => {
            match node.op.as_str() {
                "+" => eval(&node.left, stack) + eval(&node.right, stack),
                "-" => eval(&node.left, stack) - eval(&node.right, stack),
                "*" => eval(&node.left, stack) * eval(&node.right, stack),
                "/" => eval(&node.left, stack) / eval(&node.right, stack),
                "%" => eval(&node.left, stack) % eval(&node.right, stack),
                "**" => pow(eval(&node.left, stack), eval(&node.right, stack)),
                _ => panic!("Unknown operator({})", node.op),
            }
        }
        ExpressionLike::Var(var) => {
            match stack.get(var.as_str()) {
                Some(value) => *value,
                None => panic!("Var({var}) was not on a Stack\n{stack:?}"),
            }
        }
    }
}

fn exec(ast: &AST) {
    let mut stack = Stack::new();
    let AST::Lines(lines) = ast;
    for line in lines {
        match line {
            Line::Statement(stmt) => {
                let value = eval(&stmt.exp, &stack);
                stack.insert(stmt.var.as_str(), value);
            }
            Line::Expression(exp) => {
                let value = eval(exp, &stack);
                println!("{value}");
            }
        }
    }
}

fn main() {
    let program = "
    a = 1
    b = 1

    a
    tmp = a + b
    a = b
    b = tmp

    a
    tmp = a + b
    a = b
    b = tmp

    a
    tmp = a + b
    a = b
    b = tmp

    a
    tmp = a + b
    a = b
    b = tmp

    a
    tmp = a + b
    a = b
    b = tmp

    a
    tmp = a + b
    a = b
    b = tmp

    a
    tmp = a + b
    a = b
    b = tmp

    a
    tmp = a + b
    a = b
    b = tmp
    ";

    let tokens = tokenize(&program);
    let ast = parse_lines(&tokens);
    // println!("{tokens:?}");
    // println!("{ast}");

    exec(&ast);
}
