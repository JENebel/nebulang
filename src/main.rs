mod lexer;

use std::fs;

use lexer::*;

fn main() {
    let file_name = "test.nbl";
    let file = fs::read_to_string(format!("C:/Users/Joachim/VSCode Projects/nebulang/src/test_programs/{file_name}"))
        .expect("Should have been able to read the file");
    
    let f = lex(file.as_str());

    let _r = lex("if true {\n\t false 866gygg8i^^; false \n}");

    match f {
        Ok(p) => println!("{p}"),
        Err(e) => println!("{e}"),
    }
}