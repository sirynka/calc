mod display;
mod data;
mod code;

use code::{
    tokenize,
    parse,
    exec,
};

fn main() {
    let program = "

    i = 0;

    a.i = i; i = i + 1
    a.i = i; i = i + 1
    a.i = i; i = i + 1
    a.i = i; i = i + 1
    a.i = i; i = i + 1
    a.i = i; i = i + 1

    stack
    ";

    let tokens = tokenize(&program);
    let ast = parse(&tokens);
    // println!("{tokens:?}");
    println!("{ast}");
    exec(&ast);
}
