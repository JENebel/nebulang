use std::{iter::Peekable, fmt::Display};
use regex::Regex;

///Lexer token. Contains a usize: 'loc' containing the location in the input of the token
#[derive(Debug)]
pub enum LexToken {
    LeftParen(usize),
    RightParen(usize),
    Op(usize, Operator),
    Keyword(usize, Keyword),
    Int(usize, i64),
    Dec(usize, f64),
}

#[derive(Debug)]
enum Operator {
    Plus,
    Minus,
    Times,
    Div,
}

#[derive(Debug)]
enum Keyword {
    If,
    Else
}

pub struct LexedProgram {
    program: Vec<LexToken>
}

impl LexedProgram {
    pub fn new() -> Self {
        Self {
            program: Vec::new()
        }
    }

    pub fn push(&mut self, token: LexToken) {
        self.program.push(token)
    }
}

impl Display for LexedProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.program {
            write!(f, " {token:?}").expect("Should be ok here");
        }
        writeln!(f)
    }
}

pub fn lex(input: &str) -> Result<LexedProgram, String> {


    while {let haha = false; haha = true; haha} {

    }






    let mut result = LexedProgram::new();

    let mut iter = input.chars().into_iter().enumerate().peekable();

    while let Some(&c) = iter.peek(){
        let loc = c.0;
        let char = c.1;
        let rest = &input[loc..];
        let b = Regex::new(r"^(if)").unwrap().is_match(rest);
        println!("{b}");
        result.push(

            match char {
                '0'..='9' => {
                    LexToken::Int(loc, get_number(&mut iter))
                },
                '+' => LexToken::Op(loc, Operator::Plus),
                '-' => LexToken::Op(loc, Operator::Plus),
                '*' => LexToken::Op(loc, Operator::Plus),
                '/' => LexToken::Op(loc, Operator::Plus),
                _ => return Err(format!("Unknown token at loc {loc}"))
        });

        iter.next();
    }

    Ok(result)
}

fn get_word

fn get_number<T: Iterator<Item = (usize, char)>>( iter: &mut Peekable<T>) -> i64 {
    let mut n = 0;
    while let Some(&d) = iter.peek() {
        if !d.1.is_digit(10) { break }

        n *= 10;
        n += d.1.to_string().parse::<i64>().expect("Should be unreachable!");

        iter.next();
    }
    n
}