use std::{iter::Peekable, slice::Iter};

use super::*;
use lazy_static::*;

pub fn parse(lexed: Peekable<Iter<(LexToken, Location)>>) -> Result<Exp, String> {
    let mut v: Vec<Box<dyn Parser>> = Vec::new();

    //parse_paren(lexed, );

    Err("sad".to_string())
}

pub trait Parser {
    fn parse(&self, lexed: Peekable<Iter<(LexToken, Location)>>) -> Result<(Exp, Peekable<Iter<(LexToken, Location)>>), String>;
}

pub struct IntParser { }

impl Parser for IntParser {
    fn parse(&self, lexed: Peekable<Iter<(LexToken, Location)>>) -> Result<(Exp, Peekable<Iter<(LexToken, Location)>>), String> {
        let mut lexed = lexed.clone();
        match lexed.peek() {
            Some((LexToken::Int(i), loc)) => { lexed.next(); Ok((Exp::LiteralExp(Literal::Int(*i), *loc), lexed)) },
            _ => Err(format!("Expected an int at: , got ")),
        }
    }
}

fn parse_paren(lexed: Peekable<Iter<(LexToken, Location)>>) -> Result<(Exp, Peekable<Iter<(LexToken, Location)>>), String>  {
    Err(format!(""))
}

/*fn parse_int(chars: &str, index: usize) -> Result<Exp, String> {
    match chars. {
        LexToken::Int(i) => Ok(Exp::LiteralExp(Literal::Int(*i), *lexed.get_location(index))),
        _ => Err(format!("Not a number {}", chars.get_location(index)))
    }
}*/



//Generic parser
fn parse_exp<F, T> (parse: F, lexed: Peekable<Iter<(LexToken, Location)>>) -> Result<Exp, String> 
    where F: Fn(Peekable<Iter<(LexToken, Location)>>) -> Result<(Exp, Peekable<Iter<(LexToken, Location)>>), String> {
    
    let a = lexed.clone();

    Err(format!(""))
}