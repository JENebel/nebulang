use std::time::Instant;

use crate::ast::*;
use crate::lexer::*;
use crate::parser::*;
use crate::environment::*;

#[allow(dead_code)]
pub struct RunStats {
    analysis_millis: u128,
    run_millis: u128,
}

pub fn run_program(input: String, args: Vec<String>) -> Result<(Literal, RunStats), Error> {
    let mut info: bool = false;

    args.iter().for_each(|arg| {
        if arg == "--info" {
            info = true;
        }
    });


    let before = Instant::now();

    //Lex
    let lexed = match lex(&input) {
        Ok(lexed) => lexed,
        Err(err) => {
            return Err(err)
        }
    };

    let program;
    let fun_store;
    match parse(&mut lexed.iter()) {
        Ok((prog, funs)) => {
            program = prog;
            fun_store = funs;
        },
        Err(err) => {
            return Err(err)
        }
    };

    if let Err(err) = program.type_check(&mut Environment::new(fun_store.clone())) {
        return Err(err)
    }

    let analysis_time = before.elapsed().as_millis();

    if info {
        println!("Parsed in {analysis_time}ms");
        println!("Running...");
    }

    let before = Instant::now();
    let res = program.evaluate(&mut Environment::new(fun_store.clone()));
    let run_time = before.elapsed().as_millis();

    if info {
        println!("Terminated in: {run_time}ms")
    }

    match res {
        Ok(lit) => Ok((
            lit,
            RunStats {
                analysis_millis: analysis_time,
                run_millis: run_time
            }
        )),
        Err(err) => Err(err),
    }
}