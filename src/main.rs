mod lexer;
mod ast;
mod parser;

use std::fs;

use lexer::*;
use parser::*;
use ast::*;

fn main() {
    let file_name = "test.nbl";
    let file = fs::read_to_string(format!("C:/Users/Joachim/VSCode Projects/nebulang/src/test_programs/{file_name}"))
        .expect("Should have been able to read the file");
    
    let l = lex(file.as_str());

    //let l = lex("if true {\n\t false 866gygg8i^^; false \n}");

    match l {
        Ok(p) => {
            println!("{p}");
            
            let prog = parse(p.iter());

            match prog {
                Ok(res) => {
                    let program = res.0;
                    println!("{program}");
                    
                    //let res = program.evaluate();

                    //println!("Returned: {}", res)
                },
                Err(e) => println!("{} at {}", e.0, e.1),
            }
        },
        Err(e) => println!("{e}"),
    }
}