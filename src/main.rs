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
    a = 1
    b = 1

    repeat(10) {
        a
        tmp = a + b
        a = b
        b = tmp
    }
    ";

    let tokens = tokenize(&program);
    let ast = parse(&tokens);
    // println!("{tokens:?}");
    println!("{ast}");
    exec(&ast);
}
