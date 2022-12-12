use std::{iter::Peekable, fmt::Display};
use lazy_static::*;
use crate::parser::*;

///Lexer token
#[derive(Debug)]
#[derive(PartialEq)]
pub enum LexToken {
    Paren(char),
    SemiColon,
    Colon,
    Comma,

    Operator(&'static str),
    Keyword(&'static str),
    Type(&'static str),
    Id(String),

    //Literals
    Int(i64),
    Float(f64),
    Bool(bool),

    EndOfInput
}

lazy_static! {
    ///First char of every operator
    static ref OP_FIRST: Vec<char> = OPERATORS.iter().map(|op| op.chars().next().unwrap()).collect();
}

#[derive(Debug)]
#[derive(Copy, Clone, PartialEq)]
pub struct Location {
    pub line: u32,
    pub col: usize
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line: {}:{}", self.line, self.col)
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
}

impl Display for LexedProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Program:").unwrap();
        for entry in &self.program {
            let token = &entry.0;
            write!(f, " {token:?}").expect("Should be ok here");
        }
        write!(f, "")
    }
}

pub fn lex(input: &str) -> Result<LexedProgram, String> {
    let mut program = LexedProgram::new();

    let mut iter = input.chars().into_iter().enumerate().peekable();
    let mut col: usize = 1;
    let mut line = 1;
    let mut loc = Location {line, col};

    while let Some(&c) = iter.peek() {
        loc = Location {
            line, 
            col: (c.0 - col + if line == 1 {1} else {0})
        };

        let rest = &input[c.0..];

        let char = c.1;

        if char.is_alphabetic() {
            let name = get_id(&mut iter);

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
            } else if let Some(typ) = TYPES.iter().find(|typ| typ.to_string() == name) {
                //It is a type annotation
                program.push(LexToken::Type(typ), loc);
            } else {
                //It is not a keyword
                program.push(LexToken::Id(name), loc)
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
            col = c.0;
        } else if !char.is_whitespace() {

            //Match chars
            match char {
                '('|')'|'{'|'}'|'['|']' => program.push(LexToken::Paren(char), loc),
                ';' => program.push(LexToken::SemiColon, loc),
                ':' => program.push(LexToken::Colon, loc),
                ',' => program.push(LexToken::Comma, loc),

                ' ' | '\t' => {}
                _ => return Err(format!("Lexer Error: What is this '{char}' doing at {loc}?"))
            }
        }

        iter.next();
    }

    program.push(LexToken::EndOfInput, loc);

    Ok(program)
}

fn get_id<T: Iterator<Item = (usize, char)>>(iter: &mut Peekable<T>) -> String {
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