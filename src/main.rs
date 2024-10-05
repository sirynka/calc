mod display;
mod data;
mod code;
mod ops;

use code::{
    tokenize,
    parse,
    exec,
};

fn main() {
    let program = "1 + 2.1";
    let tokens = tokenize(&program);
    let ast = parse(&tokens);
    // println!("{tokens:?}");
    println!("{ast}");
    exec(&ast);
}
