use crate::data::{
    ExpressionLike,
    EolSeparated,
    Statement,
    BinaryOp,
    Keyword,
    Repeat,
    Scope,
    Stack,
    Token,
    Line,
    AST,
};

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.peek() {
        match c {
            'a'..='z' => {
                let mut token = String::new();
                while let Some(c) = chars.next_if(|c| c.is_alphabetic() || c.is_digit(10)) { token.push(c); }
                tokens.push(match token.as_str() {
                    "repeat" => Token::Repeat,
                    _ => Token::Variable(token),
                });
            }
            '0'..='9' => {
                let mut token = String::new();
                while let Some(c) = chars.next_if(|c| c.is_digit(10)) { token.push(c); }
                tokens.push(Token::Literal(token));
            }
            '+' | '-' | '/' | '%' => { if let Some(c) = chars.next() { tokens.push(Token::Op(c.to_string())); } }
            '(' => { if let Some(_) = chars.next() { tokens.push(Token::OpenParen) } }
            ')' => { if let Some(_) = chars.next() { tokens.push(Token::ClosedParen) } }
            '{' => { if let Some(_) = chars.next() { tokens.push(Token::OpenCurly) } }
            '}' => { if let Some(_) = chars.next() { tokens.push(Token::ClosedCurly) } }
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

    tokens
}

fn wrapped_in(tokens: &[Token], open: Token, close: Token) -> bool {
    tokens.len() > 2 && tokens[0] == open && tokens[tokens.len() - 1] == close
}

fn wrapped_in_parens(tokens: &[Token]) -> bool {
    wrapped_in(tokens, Token::OpenParen, Token::ClosedParen)
}

fn wrapped_in_curlys(tokens: &[Token]) -> bool {
    wrapped_in(tokens, Token::OpenCurly, Token::ClosedCurly)
}

fn strip_wrapping(tokens: &[Token], open: Token, close: Token) -> &[Token] {
    let mut tokens = tokens;
    while let (Some(_), Some(_)) = (
        tokens.strip_prefix(&[open.clone()]),
        tokens.strip_suffix(&[close.clone()])
    ) {
        tokens = &tokens[1..tokens.len() - 1];
    }
    tokens
}

fn strip_parens(tokens: &[Token]) -> &[Token] {
    strip_wrapping(tokens, Token::OpenParen, Token::ClosedParen)
}

fn strip_curlys(tokens: &[Token]) -> &[Token] {
    strip_wrapping(tokens, Token::OpenCurly, Token::ClosedCurly)
}

fn find_splits(tokens: &[Token], open: Token, close: Token, f: fn(&Token) -> bool) -> Vec<usize> {
    let mut count = 0;
    let mut splits = Vec::new();
    for (i, token) in tokens.iter().enumerate() {
        match token {
            o if *o == open => count += 1,
            c if *c == close => count -= 1,
            _ if count == 0 && f(token) => splits.push(i),
            _ => {},
        }
    }
    splits
}

fn find_closing(tokens: &[Token], open: Token, close: Token) -> usize {
    let mut count = 0;
    for (i, token) in tokens.iter().enumerate() {
        match token {
            o if *o == open => count += 1,
            c if *c == close => count -= 1,
            _ => {}
        }
        if count == 0 { return i; }
    }

    panic!("Failed to find closing paren in {tokens:?}")
}

fn parse_exp(tokens: &[Token]) -> ExpressionLike {
    let op_precedence = ["-", "+", "*", "/", "%", "**"];

    let parse_as_exp = |op_idx: usize| {
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

    let parse_as_val = || {
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
        find_splits(tokens, Token::OpenParen, Token::ClosedParen, |_| true)
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
        Some(op_idx) => parse_as_exp(op_idx),
        None => parse_as_val(),
    }
}

fn parse_stmt(tokens: &[Token]) -> Statement {
    if tokens[1] != Token::Equal {
        panic!("\n{}\n{}\n",
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

fn parse_kwrd(tokens: &[Token]) -> Keyword {
    let parse_repeat = || -> Repeat {
        let msg = format!("\n{}\n{}\n{}\n",
            "Encountered unexpected token when parsing `repeat`",
            "Usage: repeat(<expression>) { <scope> }",
            format!("Found {tokens:?}")
        );

        let tokens = &tokens[1..];
        let closing_paren = find_closing(tokens, Token::OpenParen, Token::ClosedParen);

        let count_tokens = &tokens[..=closing_paren];
        let count = match wrapped_in_parens(count_tokens) {
            true => parse_exp(strip_parens(count_tokens)),
            false => panic!("{msg}"),
        };

        let scope_tokens = &tokens[closing_paren + 1..];
        let scope = match wrapped_in_curlys(scope_tokens) {
            true => parse_scope(strip_curlys(scope_tokens)),
            false => panic!("{msg}"),
        };

        Repeat { count, scope }
    };

    match &tokens[0] {
        Token::Repeat => Keyword::Repeat(parse_repeat()),
        other => panic!("Unknown keyword({other:?})"),
    }
}

fn parse_scope(tokens: &[Token]) -> Scope {
    let find_posible_splits = |tokens: &[Token]| -> Vec<usize> {
        find_splits(tokens, Token::OpenCurly, Token::ClosedCurly, |tok| tok == &Token::EndOfLine)
    };

    let parse_line = |line: &[Token]| -> Option<Line> {
        if line.is_empty() { return None; }

        match line.contains(&Token::Equal) {
            true => Some(Line::Statement(parse_stmt(line))),
            false => Some(Line::Expression(parse_exp(line))),
        }
    };

    let parse_eol_separated = |tokens: &[Token]| -> Option<EolSeparated> {
        if wrapped_in_curlys(tokens) {
            return Some(EolSeparated::Scope(parse_scope(strip_curlys(tokens))))
        }

        match tokens.first() {
            Some(Token::Repeat) => return Some(EolSeparated::Keyword(parse_kwrd(tokens))),
            _ => if let Some(line) = parse_line(tokens) {
                return Some(EolSeparated::Line(line));
            },
        };
        None
    };

    let mut scope: Vec<EolSeparated> = Vec::new();
    let line_endings = find_posible_splits(tokens);
    let zipped_line_endings = line_endings.windows(2);

    if let Some(p) = line_endings.first() {
        if let Some(line) = parse_eol_separated(&tokens[..*p]) {
            scope.push(line);
        }
    }

    for w in zipped_line_endings {
        if let [b, e] = w {
            if let Some(line) = parse_eol_separated(&tokens[*b + 1..*e]) {
                scope.push(line);
            }
        }
    }

    if let Some(p) = line_endings.last() {
        if let Some(line) = parse_eol_separated(&tokens[*p + 1..]) {
            scope.push(line);
        }
    }

    Scope { inner: scope }
}

pub fn parse(tokens: &[Token]) -> AST {
    AST::Scope(parse_scope(tokens))
}

fn pow(base: i64, exp: i64) -> i64 {
    let mut res = 1;
    for _ in 0..exp { res *= base; }
    res
}

fn exec_expr(node: &ExpressionLike, stack: &Stack) -> i64 {
    match node {
        ExpressionLike::Empty => 0,
        ExpressionLike::Val(num) => num.parse().unwrap(),
        ExpressionLike::Exp(node) => {
            match node.op.as_str() {
                "+" => exec_expr(&node.left, stack) + exec_expr(&node.right, stack),
                "-" => exec_expr(&node.left, stack) - exec_expr(&node.right, stack),
                "*" => exec_expr(&node.left, stack) * exec_expr(&node.right, stack),
                "/" => exec_expr(&node.left, stack) / exec_expr(&node.right, stack),
                "%" => exec_expr(&node.left, stack) % exec_expr(&node.right, stack),
                "**" => pow(exec_expr(&node.left, stack), exec_expr(&node.right, stack)),
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

pub fn exec(ast: &AST) {
    fn exec_keyword(kwrd: &Keyword, stack: &mut Stack) {
        match kwrd {
            Keyword::Repeat(repeat) => {
                for _ in 0..exec_expr(&repeat.count, stack) {
                    exec_scope(&repeat.scope, stack);
                }
            }
        }
    }

    fn exec_line(line: &Line, stack: &mut Stack) {
        match line {
            Line::Statement(stmt) => {
                let value = exec_expr(&stmt.exp, stack);
                stack.insert(stmt.var.clone(), value);
            }
            Line::Expression(exp) => {
                let value = exec_expr(exp, stack);
                println!("{value}");
            }
        }
    }

    fn stack_copy(local_stack: &Stack, stack: &mut Stack) {
        for (var, val) in stack.iter_mut() {
            let local_val = local_stack.get(var.as_str());
            if let Some(local_val) = local_val {
                *val = *local_val;
            }
        }
    }

    fn exec_scope(scope: &Scope, stack: &mut Stack) {
        for inner in &scope.inner {
            match inner {
                EolSeparated::Line(line) => exec_line(line, stack),
                EolSeparated::Keyword(kwrd) => exec_keyword(kwrd, stack),
                EolSeparated::Scope(scope) => {
                    let mut local_stack = stack.clone();
                    exec_scope(scope, &mut local_stack);
                    stack_copy(&local_stack, stack);
                },
            }
        }
    }

    let mut stack = Stack::new();
    let AST::Scope(scope) = ast;
    exec_scope(scope, &mut stack);
}
