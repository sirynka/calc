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
    a = 0
    i = 0

    repeat(10) {
        a.i = i
        i = i + 1
    }

    stack
    ";

    let tokens = tokenize(&program);
    let ast = parse(&tokens);
    // println!("{tokens:?}");
    println!("{ast}");
    exec(&ast);
}
