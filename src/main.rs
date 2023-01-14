extern crate nebulang;

use std::{fs, env, io::{self, Read, Write}, process::exit};

use nebulang::runner::*;

fn main() {
    let mut args = std::env::args();
    args.next();
    let path = match args.next() {
        Some(file) => env::current_dir().unwrap().join(file).with_extension("nbl"),
        None => { 
            println!("Please provide a file");
            return; 
        }
    };

    let input = match fs::read_to_string(path.clone()) {
        Ok(file_contents) => file_contents,
        Err(_) => {
            println!("Could not open file: \"{}\"", path.display());
            finish();
            "".to_string()
        },
    };
    
    match run_program(input, args.collect::<Vec<String>>()) {
        Ok(res) => println!("Returned: {}", res.0),
        Err(err) => println!("{err}"),
    }

    finish()
}

fn finish() {
    println!("Press enter to close...");

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    stdout.flush().unwrap();
    let _ = stdin.read(&mut [0u8]).unwrap();
    exit(0);
}