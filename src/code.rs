use crate::data::{
    ExpressionLike,
    EolSeparated,
    Statement,
    BinaryOp,
    VarName,
    Literal,
    Keyword,
    Repeat,
    Scope,
    Stack,
    Token,
    While,
    Line,
    AST,
    If,
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
                    "while" => Token::While,
                    "if" => Token::If,
                    "stack" => Token::Stack,
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
            '.' => { if let Some(_) = chars.next() { tokens.push(Token::Dot) } }
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

fn parse_var_name(tokens: &[Token]) -> VarName {
    let Token::Variable(name) = &tokens[0] else { unreachable!() };
    let name = name.to_string();

    match tokens.iter().position(|t| t == &Token::Dot) {
        Some(dot) => VarName::Indexable(
            /* name: */ name,
            /* idx:  */ Box::new(parse_exp(&tokens[dot+1..]))
        ),
        None if tokens.len() == 1 => VarName::Simple(name),
        None => panic!("\n{}{}{}\n",
            "Encountered unexpected token when parsing variable name\n",
            "Usage: <variable> or <variable>.<expression>\n",
            format!("Found {tokens:?}")
        )
    }
}

fn parse_literal(tokens: &[Token]) -> Literal {
    match tokens.len() {
        1 => {
            let Token::Literal(num) = &tokens[0] else { unreachable!() };
            let num: i64 = num.parse().unwrap();
            Literal::I64(num)
        },
        3 => {
            let Token::Literal(whole) = &tokens[0] else { unreachable!() };
            let Token::Dot = &tokens[1] else { unreachable!() };
            let Token::Literal(fraction) = &tokens[2] else { unreachable!() };

            let num = format!("{whole}.{fraction}");
            let num: f64 = num.parse().unwrap();
            Literal::F64(num)
        }
        _ => panic!("\n{}{}{}\n",
            "Encountered unexpected token when parsing a literal\n",
            "Usage: <number> or <number>.<number>\n",
            format!("Found {tokens:?}")
        ),
    }
}

fn parse_exp(tokens: &[Token]) -> ExpressionLike {
    let op_precedence = ["-", "+", "*", "/", "%", "**"];

    let parse_as_exp = |op_idx: usize| {
        let Token::Op(op) = tokens[op_idx].clone() else { unreachable!() };

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
        if wrapped_in_parens(tokens) { return parse_exp(strip_parens(tokens)); }

        match &tokens[0] {
            Token::Literal(_) => ExpressionLike::Val(parse_literal(tokens)),
            Token::Variable(_) => ExpressionLike::Var(parse_var_name(tokens)),
            _ => panic!("\n{}{}{}\n",
                "Encountered unexpected token when parsing an expression\n",
                "Usage: <expression> <operator> <expression>\n",
                format!("Found {tokens:?}")
            ),
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
    let Some(equal) = tokens.iter().position(|t| t == &Token::Equal) else { unreachable!() };

    Statement {
        var: parse_var_name(&tokens[..equal]),
        exp: parse_exp(&tokens[equal+1..]),
    }
}

fn parse_kwrd(tokens: &[Token]) -> Keyword {
    let parse_keyword_template = |keyword| -> (ExpressionLike, Scope) {
        let msg = format!("\n{}{}{}\n",
            format!("Encountered unexpected token when parsing `{keyword}`\n"),
            format!("Usage: {keyword} (<expression>) {{ <scope> }}\n"),
            format!("Found {tokens:?}")
        );

        let tokens = &tokens[1..];
        let closing_paren = find_closing(tokens, Token::OpenParen, Token::ClosedParen);

        let condition_tokens = &tokens[..=closing_paren];
        let condition = match wrapped_in_parens(condition_tokens) {
            true => parse_exp(strip_parens(condition_tokens)),
            false => panic!("{msg}"),
        };

        let scope_tokens = &tokens[closing_paren + 1..];
        let scope = match wrapped_in_curlys(scope_tokens) {
            true => parse_scope(strip_curlys(scope_tokens)),
            false => panic!("{msg}"),
        };

        (condition, scope)
    };

    let parse_repeat = || -> Repeat {
        let (count, scope) = parse_keyword_template("repeat");
        Repeat { count, scope }
    };

    let parse_while = || -> While {
        let (condition, scope) = parse_keyword_template("while");
        While { condition, scope }
    };

    let parse_if = || -> If {
        let (condition, scope) = parse_keyword_template("if");
        If { condition, scope }
    };

    match &tokens[0] {
        Token::Repeat => Keyword::Repeat(parse_repeat()),
        Token::While => Keyword::While(parse_while()),
        Token::If => Keyword::If(parse_if()),
        Token::Stack => Keyword::Stack,
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
            Some(
                Token::Stack |
                Token::Repeat |
                Token::While |
                Token::If
            ) => return Some(EolSeparated::Keyword(parse_kwrd(tokens))),
            _ => if let Some(line) = parse_line(tokens) {
                return Some(EolSeparated::Line(line));
            },
        };
        None
    };

    let mut scope: Vec<EolSeparated> = Vec::new();
    let line_endings = find_posible_splits(tokens);
    let zipped_line_endings = line_endings.windows(2);

    if line_endings.is_empty() {
        if let Some(line) = parse_eol_separated(tokens) {
            scope.push(line);
        }
    }

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

fn exec_expr(node: &ExpressionLike, stack: &Stack) -> Literal {
    match node {
        ExpressionLike::Val(literal) => literal.clone(),
        ExpressionLike::Exp(node) => {
            match node.op.as_str() {
                "+"  => Literal::add(exec_expr(&node.left, stack), exec_expr(&node.right, stack)),
                "-"  => Literal::sub(exec_expr(&node.left, stack), exec_expr(&node.right, stack)),
                "*"  => Literal::mul(exec_expr(&node.left, stack), exec_expr(&node.right, stack)),
                "/"  => Literal::div(exec_expr(&node.left, stack), exec_expr(&node.right, stack)),
                "**" => Literal::pow(exec_expr(&node.left, stack), exec_expr(&node.right, stack)),
                "%"  => Literal::modulo(exec_expr(&node.left, stack), exec_expr(&node.right, stack)),
                _ => panic!("Unknown operator({})", node.op),
            }
        }
        ExpressionLike::Var(var) => {
            let name = &format!("{var}");
            match stack.get(name) {
                Some(value) => value.clone(),
                None => panic!("Var({var}) was not on a Stack\n{stack:?}"),
            }
        }
    }
}

pub fn exec(ast: &AST) {
    fn exec_keyword(kwrd: &Keyword, stack: &mut Stack) {
        match kwrd {
            Keyword::Repeat(repeat) => {
                for _ in 0..exec_expr(&repeat.count, stack).as_i64() {
                    exec_scope(&repeat.scope, stack);
                }
            }
            Keyword::While(whhile) => {
                while exec_expr(&whhile.condition, stack).is_zero() {
                    exec_scope(&whhile.scope, stack);
                }
            }
            Keyword::If(iff) => {
                if exec_expr(&iff.condition, stack).is_zero() {
                    exec_scope(&iff.scope, stack);
                }
            }
            Keyword::Stack => println!("{stack:#?}"),
        }
    }

    fn exec_line(line: &Line, stack: &mut Stack) {
        match line {
            Line::Statement(stmt) => {
                let value = exec_expr(&stmt.exp, stack);
                let name = match &stmt.var {
                    VarName::Simple(name) => name.to_string(),
                    VarName::Indexable(name, exp) => {
                        let value = exec_expr(&exp, stack);
                        format!("{name}.{value}")
                    },
                };
                stack.insert(name, value);
            }
            Line::Expression(exp) => {
                let value = exec_expr(exp, stack);
                println!("{value}");
            }
        }
    }

    fn stack_copy(local_stack: Stack, stack: &mut Stack) {
        for (var, val) in local_stack.into_iter() {
            match var.split_once('.') {
                // a hack to copy 'array' to outer scope
                Some((prefix, _)) => if !stack.contains_key(prefix) {
                    stack.insert(var, val);
                }
                None => if !stack.contains_key(&var) {
                    stack.insert(var, val);
                }
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
                    stack_copy(local_stack, stack);
                },
            }
        }
    }

    let mut stack = Stack::new();
    let AST::Scope(scope) = ast;
    exec_scope(scope, &mut stack);
}
