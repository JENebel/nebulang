use std::iter::Peekable;

use super::*;
use lazy_static::*;

//Parse rules
lazy_static! {
    ///All tokens terminating a statement
    pub static ref TERMINATORS: Vec<LexToken> = Vec::from([LexToken::SemiColon, LexToken::Paren('}')]);
}

pub fn parse(lexed: LexedProgram) -> Result<Exp, String> {
    let mut iter = lexed.iter();

    parse_block(&mut iter)
}

fn parse_block(iter: &mut Peekable<std::slice::Iter<(LexToken, Location)>>) -> Result<Exp, String> {
    
    while let Some(token) = iter.peek() {
        if token.0 == LexToken::Paren('}') { break }

        
    }


    Ok(Exp::BlockExp(Vec::new()))
}

fn parse_statement(iter: &mut Peekable<std::slice::Iter<(LexToken, Location)>>) -> Result<Exp, String> {

    Ok(Exp::BlockExp(Vec::new()))
}