use std::{fs, time::Instant, env, io::{self, Read, Write}};

mod lexer;
mod ast;
mod parser;
mod eval;
mod type_checker;
mod environment;
mod runner;
mod tests;

use lexer::*;
use parser::*;
use ast::*;
use environment::*;
use runner::*;

fn main() {
    let mut args = std::env::args();
    args.next();
    let path = match args.next() {
        Some(file) => env::current_dir().unwrap().join(file),
        None => { 
            println!("Please provide a file");
            return; 
        }
    };

    println!("{}", path.display());

    let file = fs::read_to_string(path).expect("Could not read the file");
    
    match run_program(file, args.collect::<Vec<String>>()) {
        Ok(res) => println!("Returned: {}", res.0),
        Err(err) => println!("{err}"),
    }

    println!("Press enter to close...");

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    stdout.flush().unwrap();
    let _ = stdin.read(&mut [0u8]).unwrap();
}