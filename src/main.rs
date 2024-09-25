#[derive(Debug, PartialEq, Clone)]
enum Token {
    Literal(String),
    Op(String),
    OpenParen,
    ClosedParen
}

fn tokenize(s: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.peek() {
        match c {
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
    Val(String),
    Exp(Box<Node>),
    Empty,
}

impl std::fmt::Display for ValOrExp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValOrExp::Empty => write!(f, "0"),
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
                let Token::Literal(num) = tokens[0].clone() else { unreachable!(); };
                ValOrExp::Val(num)
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

    let closed_paren_idx = || {
        if tokens.first() == Some(&Token::OpenParen) {
            let mut count = 0;
            for (i, token) in tokens.iter().enumerate() {
                match token {
                    Token::OpenParen => count += 1,
                    Token::ClosedParen => count -= 1,
                    _ => {}
                }
                if count == 0 { return i; }
            }
        }

        0
    };

    let skip = closed_paren_idx();
    let is_op = |t: &Token| if let Token::Op(_) = t { true } else { false };
    let op_pos = tokens.iter().skip(skip).position(is_op);

    match op_pos {
        Some(op_idx) => parse_tokens_as_exp(op_idx + skip),
        None => parse_tokens_as_val(),
    }
}

fn pow(base: i64, exp: i64) -> i64 {
    let mut res = 1;
    for _ in 0..exp { res *= base; }
    res
}

fn eval(node: &ValOrExp) -> i64 {
    match node {
        ValOrExp::Empty => 0,
        ValOrExp::Val(num) => num.parse().unwrap(),
        ValOrExp::Exp(node) => {
            match node.op.as_str() {
                "+" => eval(&node.left) + eval(&node.right),
                "-" => eval(&node.left) - eval(&node.right),
                "*" => eval(&node.left) * eval(&node.right),
                "/" => eval(&node.left) / eval(&node.right),
                "%" => eval(&node.left) % eval(&node.right),
                "**" => pow(eval(&node.left), eval(&node.right)),
                _ => panic!("Unknown operator({})", node.op),
            }
        }
    }
}

fn main() {
    let s = "(1+2) * (3+4)";
    let tokens = tokenize(s);
    let ast = ast(&tokens);
    let res = eval(&ast);
    println!("{:?}", tokens);
    println!("{ast} = {res}");
}
