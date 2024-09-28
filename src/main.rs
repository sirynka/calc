type Stack<'a> = std::collections::HashMap<&'a str, i64>;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Literal(String),
    Variable(String),
    Op(String),
    OpenParen,
    ClosedParen
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

    tokens
}

#[derive(Debug, PartialEq)]
enum ValOrExp {
    Var(String),
    Val(String),
    Exp(Box<Node>),
    Empty,
}

impl std::fmt::Display for ValOrExp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValOrExp::Empty => write!(f, "0"),
            ValOrExp::Var(var) => write!(f, "{var}"),
            ValOrExp::Val(num) => write!(f, "{num}"),
            ValOrExp::Exp(node) => write!(f, "({} {} {})", node.left, node.op, node.right),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Node {
    op: String,
    left: ValOrExp,
    right: ValOrExp,
}

fn ast(tokens: &[Token]) -> ValOrExp {
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

        ValOrExp::Exp(
            Box::new(
                Node {
                    op,
                    left: ast(&tokens[..op_idx]),
                    right: ast(&tokens[op_idx + 1..]),
                }
            )
        )
    };

    let parse_tokens_as_val = || {
        match tokens.len() {
            0 => return ValOrExp::Empty,
            1 => return {
                match &tokens[0] {
                    Token::Literal(num) => ValOrExp::Val(num.clone()),
                    Token::Variable(var) => ValOrExp::Var(var.clone()),
                    _ => unreachable!(),
                }
            },
            _ => {
                if wrapped_in_parens(tokens) { return ast(strip_parens(tokens)); }
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

fn pow(base: i64, exp: i64) -> i64 {
    let mut res = 1;
    for _ in 0..exp { res *= base; }
    res
}

fn eval(node: &ValOrExp, stack: &Stack) -> i64 {
    match node {
        ValOrExp::Empty => 0,
        ValOrExp::Val(num) => num.parse().unwrap(),
        ValOrExp::Exp(node) => {
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
        ValOrExp::Var(var) => {
            match stack.get(var.as_str()) {
                Some(value) => *value,
                None => panic!("Var({var}) was not on a Stack\n{stack:?}"),
            }
        }
    }
}

fn execute(program: &str) {
    let mut stack = Stack::new();

    let exec_exp = |exp: &str, stack: &Stack| -> i64 {
        let tokens = tokenize(exp);
        let ast = ast(&tokens);
        let value = eval(&ast, &stack);
        value
    };

    fn skip_empty(line: &str) -> Option<&str> {
        let line = line.trim();
        if line.is_empty() { None } else { Some(line) }
    }

    for line in program.lines().filter_map(skip_empty) {
        match line.split_once("=") {
            Some((var, exp)) => {
                let value = exec_exp(exp, &stack);
                stack.insert(var.trim(), value);
            }
            None => {
                let value = exec_exp(line, &stack);
                println!("{}", value);
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

    execute(program);
}
