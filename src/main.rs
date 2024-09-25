#[derive(Debug, Eq, PartialEq)]
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

fn main() {
    let s = "123 ** 456";
    let tokens = tokenize(s);
    println!("{:?}", tokens);
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