#[derive(Debug, PartialEq, Clone)]
enum Token {
    Literal(String),
    Op(String),
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
            '+' | '-' | '/' => {
                if let Some(c) = chars.next() {
                    tokens.push(Token::Op(c.to_string()));
                }
            }
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

#[derive(Debug, PartialEq)]
struct Node {
    op: String,
    left: ValOrExp,
    right: ValOrExp,
}

fn ast(tokens: &[Token]) -> ValOrExp {
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
            _ => panic!("This list of tokens does not contain a single operator, those it should only contain a single token (or noting) but found {tokens:?}"),
        }
    };

    let is_op = |t: &Token| if let Token::Op(_) = t { true } else { false };
    match tokens.iter().position(is_op) {
        Some(op_idx) => parse_tokens_as_exp(op_idx),
        None => parse_tokens_as_val(),
    }
}

fn main() {
    let s = "2 * 2 + 2";
    let tokens = tokenize(s);
    println!("{:?}", tokens);
    let ast = ast(&tokens);
    println!("{:#?}", ast);
}

#[test]
fn plus() {
    for s in [
        "123+456",
        "123 + 456",
        "123 + 456 ",
        "123   +   456",
    ] {
        let mut tokens = tokenize(s).into_iter();
        assert_eq!(tokens.next(), Some(Token::Literal(String::from("123"))));
        assert_eq!(tokens.next(), Some(Token::Op(String::from("+"))));
        assert_eq!(tokens.next(), Some(Token::Literal(String::from("456"))));
    }
}

#[test]
fn stars() {
    {
        let mut tokens = tokenize("123**456").into_iter();
        assert_eq!(tokens.next(), Some(Token::Literal(String::from("123"))));
        assert_eq!(tokens.next(), Some(Token::Op(String::from("**"))));
        assert_eq!(tokens.next(), Some(Token::Literal(String::from("456"))));
    }

    {
        let mut tokens = tokenize("123***456").into_iter();
        assert_eq!(tokens.next(), Some(Token::Literal(String::from("123"))));
        assert_eq!(tokens.next(), Some(Token::Op(String::from("**"))));
        assert_eq!(tokens.next(), Some(Token::Op(String::from("*"))));
        assert_eq!(tokens.next(), Some(Token::Literal(String::from("456"))));
    }
}