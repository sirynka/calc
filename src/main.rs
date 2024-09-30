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
    a = 10
    while (a) {
        a = a - 1
        if (a % 2) {
            a
        }
    }
    ";

    let tokens = tokenize(&program);
    let ast = parse(&tokens);
    // println!("{tokens:?}");
    println!("{ast}");
    exec(&ast);
}
