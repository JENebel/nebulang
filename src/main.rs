mod lexer;

use lexer::*;

fn main() {
    let res = lex("if12345").unwrap();
    println!("{res}")
}
