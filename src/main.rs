mod display;
mod data;
mod code;

use code::{exec, parse, tokenize};

fn main() {
    // let program = "
    // a = 1
    // b = 1
    // a

    // tmp = a + b; a = b; b = tmp; a
    // tmp = a + b; a = b; b = tmp; a
    // tmp = a + b; a = b; b = tmp; a
    // tmp = a + b; a = b; b = tmp; a
    // tmp = a + b; a = b; b = tmp; a
    // tmp = a + b; a = b; b = tmp; a
    // tmp = a + b; a = b; b = tmp; a
    // tmp = a + b; a = b; b = tmp; a
    // tmp = a + b; a = b; b = tmp; a
    // ";

    // let program = "1; 2; 3";

    let program = "
    a = 1
    { b = 2; c = 3 }
    d = 4
    ";

    let tokens = tokenize(&program);
    let ast = parse(&tokens);
    // println!("{tokens:?}");
    println!("{ast}");
    exec(&ast);
}
