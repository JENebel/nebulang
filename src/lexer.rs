use std::{iter::Peekable, fmt::Display};
use lazy_static::*;

///Lexer token
#[derive(Debug)]
#[derive(PartialEq)]
pub enum LexToken {
    Paren(char),
    SemiColon,
    Operator(&'static str),
    Keyword(&'static str),
    Name(String),

    //Literals
    Int(i64),
    Float(f64),
    Bool(bool),

    //Only for parser use
    AnyExp
}

lazy_static! {
    ///All legal operators
    pub static ref OPERATORS: Vec<&'static str> = Vec::from(["+", "-", "*", "/", "<=", ">=", "<", ">", "==", "="]);

    ///All legal keywords
    pub static ref KEYWORDS: Vec<&'static str> = Vec::from(["if", "else", "while", "for"]);



    ///First char of every operator
    static ref OP_FIRST: Vec<char> = OPERATORS.iter().map(|op| op.chars().next().unwrap()).collect();
}

pub struct Location {
    line: u32,
    loc: u32
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}:{}", self.line, self.loc)
    }
}

pub struct LexedProgram {
    program: Vec<(LexToken, Location)>,
}

impl LexedProgram {
    pub fn new() -> Self {
        Self {
            program: Vec::new()
        }
    }

    pub fn push(&mut self, token: LexToken, location: Location) {
        self.program.push((token, location))
    }

    pub fn iter(&self) -> Peekable<std::slice::Iter<(LexToken, Location)>> {
        self.program.iter().peekable()
    }

    pub fn get_token(&self, index: usize) -> &LexToken {
        &self.program[index].0
    }

    pub fn get_location(&self, index: usize) -> &Location {
        &self.program[index].1
    }
}

impl Display for LexedProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Program:").unwrap();
        for entry in &self.program {
            let token = &entry.0;
            write!(f, " {token:?}").expect("Should be ok here");
        }
        writeln!(f)
    }
}

pub fn lex(input: &str) -> Result<LexedProgram, String> {
    let mut program = LexedProgram::new();

    let mut iter = input.chars().into_iter().enumerate().peekable();
    let mut line_loc = 0;
    let mut line = 1;

    while let Some(&c) = iter.peek() {
        let loc = Location {
            line, 
            loc: (c.0 - line_loc) as u32
        };

        let rest = &input[c.0..];

        let char = c.1;

        if char.is_alphabetic() {
            let name = get_name(&mut iter);

            //Special cases
            if name == "true" {
                program.push(LexToken::Bool(true), loc);
            }
            else if name == "false" {
                program.push(LexToken::Bool(false), loc);
            }

            //Keywords, names and types
            else if let Some(kwd) = KEYWORDS.iter().find(|kwd| kwd.to_string() == name) {
                //It is a keyword
                program.push(LexToken::Keyword(kwd), loc);
            } else {
                //It is not a keyword
                program.push(LexToken::Name(name), loc)
            }
            
            continue
        }

        //Match operator
        if OP_FIRST.iter().any(|c| char == *c) {
            program.push(LexToken::Operator(get_operator(&mut iter, rest)), loc);
            continue
        }

        //Match number
        if char.is_numeric() {
            program.push(get_number(&mut iter), loc);
            continue
        }

        if char == '\n' {
            line += 1;
                line_loc = c.0;
        } else if !char.is_whitespace() {

            //Match chars
            match char {
                '('|')'|'{'|'}' => program.push(LexToken::Paren(char), loc),
                ';' => program.push(LexToken::SemiColon, loc),

                ' ' | '\t' => {}
                _ => return Err(format!("Lexer Error: What is this '{char}' doing at {loc}?"))
            }
        }

        iter.next();
    }

    Ok(program)
}

fn get_name<T: Iterator<Item = (usize, char)>>(iter: &mut Peekable<T>) -> String {
    let mut res = String::new();
    while iter.peek().is_some() && iter.peek().unwrap().1.is_alphanumeric() {
        res = format!("{res}{}", iter.next().unwrap().1);
    }

    return res
}

fn get_operator<T: Iterator<Item = (usize, char)>>(iter: &mut Peekable<T>, rest: &str) -> &'static str {
    for op in OPERATORS.iter() {
        if rest.starts_with(op) {
            for _ in 0..op.len() {
                iter.next();
            }
            return op
        }
    }

    unreachable!()
}

fn get_number<T: Iterator<Item = (usize, char)>>(iter: &mut Peekable<T>) -> LexToken {
    let mut res = String::new();
    while iter.peek().is_some() && iter.peek().unwrap().1.is_digit(10) {
        res = format!("{res}{}", iter.next().unwrap().1);
    }
    if iter.peek().is_none() || iter.peek().unwrap().1 != '.' {
        return LexToken::Int(res.parse::<i64>().unwrap())
    }
    //Floats
    iter.next();

    res = format!("{res}.");

    while iter.peek().is_some() && iter.peek().unwrap().1.is_digit(10) {
        res = format!("{res}{}", iter.next().unwrap().1);
    }
    
    LexToken::Float(res.parse::<f64>().unwrap())
}