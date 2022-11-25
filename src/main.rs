mod lexer;
mod ast;
mod parser;
mod eval;

use std::{fs, time::Instant};

use lexer::*;
use parser::*;
use ast::*;
use eval::*;
use simple_process_stats::ProcessStats;

#[async_std::main]
async fn main() {
    let before = Instant::now();

    let file_name = "primes.nbl";
    let file = fs::read_to_string(format!("C:/Users/Joachim/Documents/VSCode/nebulang/src/test_programs/{file_name}"))
        .expect("Should have been able to read the file");
    
    let mem_before = ProcessStats::get().await.unwrap().memory_usage_bytes;

    let l = lex(file.as_str());

    //let l = lex("0");

    match l {
        Ok(p) => {
            let prog = parse(&mut p.iter());

            match prog {
                Ok(program) => {
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
                },
                Err(e) => println!("{} at {}", e.0, e.1),
            }
        },
        Err(e) => println!("{e}"),
    }
}