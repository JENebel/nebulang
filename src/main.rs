mod lexer;
mod ast;
mod parser;
mod eval;
mod type_checker;
mod environment;

use std::{fs, time::Instant};

use async_std::io::{stdin, ReadExt};
use lexer::*;
use parser::*;
use ast::*;
use eval::*;
use simple_process_stats::ProcessStats;
use type_checker::*;
use environment::*;

#[async_std::main]
async fn main() {
    let mut args = std::env::args();
    args.next();
    let path = match args.next() {
        Some(path) => path,
        None => {
            let file_name = "test";
            format!("C:/Users/Joachim/Documents/VSCode/nebulang/src/test_programs/{file_name}.nbl")
        }
    };

    let file = fs::read_to_string(path)
        .expect("Should have been able to read the file");
    
    let before = Instant::now();
    let mem_before = ProcessStats::get().await.unwrap().memory_usage_bytes;

    //Lex
    let lexed = match lex(file.as_str()) {
        Ok(lexed) => lexed,
        Err((msg, loc)) => {
            println!("Lexer Error: {msg}. At: {loc}"); 
            return
        }
    };

    //Parse
    let mut program = match parse(&mut lexed.iter()) {
        Ok(program) => program,
        Err((msg, loc)) => {
            println!("Parse Error: {msg}. At: {loc}"); 
            return
        }
    };

    //Type check
    if let Err((msg, loc)) = program.type_check(&mut Environment::new()) {
        println!("Type Error: {msg}. At: {loc}"); 
        return
    }

    //Run
    let mem_after = ProcessStats::get().await.unwrap().memory_usage_bytes;

    let total_mem = (mem_after - mem_before) / 1_028;

    let elapsed = before.elapsed().as_millis();

    println!("Parsed in {elapsed}ms");
    println!("Program size: {}kB", total_mem);
    println!("Running");
    //println!("------------------------\n");

    let before = Instant::now();
    let res = program.evaluate(&mut Environment::new());
    let elapsed = before.elapsed().as_millis();

    //println!("\n------------------------");
    println!("Returned: {res}");
    println!("Time: {elapsed}ms");
}