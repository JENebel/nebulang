use core::panic;
use std::{iter::Peekable, slice::Iter};

use lazy_static::lazy_static;

use super::*;

use LexToken::*;

type LexIter<'a> = Peekable<Iter<'a, (LexToken, Location)>>;
type ParseRes<'a> = Result<(Exp, LexIter<'a>), (String, Location)>;

lazy_static!(
    ///All legal operators
    pub static ref OPERATORS: Vec<&'static str> = Vec::from(["+=", "-=", "+", "-", "*", "/", "%", "<=", ">=", "<", ">", "!", "==", "!=", "="]);

    ///All legal keywords
    pub static ref KEYWORDS: Vec<&'static str> = Vec::from(["if", "else", "while", "for"]);

    pub static ref PRECEDENCE: Vec<Vec<LexToken>> = vec![
        //Unary
        vec![Operator("-"), Operator("!")],

        //Binary
        vec![Operator("*"), Operator("/"), Operator("%")],
        vec![Operator("+"), Operator("-")],
        vec![Operator("<"), Operator(">"), Operator("<="), Operator(">=")],
        vec![Operator("="), Operator("+="), Operator("-=")],
    ];

    pub static ref TERMINATORS: Vec<LexToken> = vec![
        Paren(';'),
        Paren(')'),
        Paren('}'),
        Paren(']'),
    ];
);

#[derive(Debug)]
enum Term<'a> {
    OpTerm(&'a LexToken),
    ExpTerm(Exp)
}

pub fn parse(lexed: LexIter) -> ParseRes {
    parse_exp(lexed)
}

fn parse_exp(lexed: LexIter) -> ParseRes {
    let mut lexed = lexed.clone();
    let mut terms: Vec<(Term, Location)> = Vec::new();

    while let Some(peek) = lexed.peek() {
        let token = &peek.0;
        let loc = &peek.1;

        if TERMINATORS.contains(token) {
            if token == &SemiColon {
                lexed.next();
            }
            break;
        }

        match token {
            Keyword(kwd) => {
                match match *kwd {
                    "if" => parse_if(lexed),
                    "while" => parse_while(lexed),
                    "for" => parse_for(lexed),
                    _ => Err((format!("Unknown keyword '{kwd}'"), *loc))
                } {
                    Ok(_) => {todo!()},
                    Err(_) => {todo!()},
                }
            }
            Paren('(') => {
                lexed.next();

                match parse_exp(lexed) {
                    Ok((exp, after)) => {
                        match parenthesis(after, ')') {
                            Ok(rest) => {
                                terms.push((Term::ExpTerm(exp), *loc));
                                lexed = rest;
                            },
                            Err(err) => return Err(err),
                        }
                    },
                    Err(err) => return Err(err),
                };
            },
            Paren('{') => todo!(),//match parse_block(lexed) {},
            LexToken::Operator(_) => {
                if PRECEDENCE[0].contains(token) {
                    //Is unary
                    lexed.next();

                    match parse_exp(lexed) {
                        Ok((exp, rest)) => {
                            terms.push((Term::ExpTerm(exp), *loc));
                            lexed = rest;
                        }
                        Err(err) => return Err(err)
                    }
                }
                else {
                    //Binary
                    terms.push((Term::OpTerm(token), *loc));
                    lexed.next();
                }
            },
            Name(_) => todo!(),
            Int(_) => match parse_int(lexed) {
                Ok((exp, rest)) => {
                    terms.push((Term::ExpTerm(exp), *loc));
                    lexed = rest;
                },
                Err(err) => return Err(err),
            },
            Float(_) => todo!(),
            Bool(_) => todo!(),
            _ => todo!(),
        };
    }

    //Make tree
    //Transform to references
    let mut transformed = Vec::new();
    for i in 0..terms.len() {
        transformed.push((&terms[i].0, &terms[i].1))
    }
    
    match construct_exp_tree(transformed.iter().peekable()) {
        Ok(exp) => Ok((exp, lexed)),
        Err(err) => Err(err),
    }
}

fn construct_exp_tree<'a>(terms: Peekable<std::slice::Iter<'_, (&Term<'_>, &lexer::Location)>>) -> Result<Exp, (String, Location)> {
    let mut terms = terms.clone();
    //This is an atomic node
    if terms.len() == 1 {
        return match terms.next() {
            Some(term) => match term {
                (Term::OpTerm(op), loc) => Err((format!("Expected an expression, found {op:?}"), **loc)),
                (Term::ExpTerm(exp), _) => Ok(exp.clone()),
            },
            None => unreachable!(),
        }
    }

    //Unary
    if let Some((Term::OpTerm(op), loc)) = terms.peek() {
        //Abort if it is not a unary operator at all
        if !PRECEDENCE[0].contains(op) {
            return Err((format!("{op:?} is not a unary operator"), **loc))
        }

        terms.next();

        return match construct_exp_tree(terms) {
            Ok(exp) => Ok(Exp::UnOpExp(parse_operator(*op), Box::new(exp), **loc)),
            Err(err) => Err(err),
        }
    }

    //Res
    //Skip unary operators as they are handled, and reverse to split at weakest operators first
    for level in (1..PRECEDENCE.len()).rev() {
        let mut terms = terms.clone();
        let mut left_side: Vec<(&Term, &Location)> = Vec::new();
        while let Some(entry) = terms.peek() {
            let term = &entry.0;
            let loc = &entry.1;
            //println!("left_side: {left_side:?}");
            match term {
                Term::OpTerm(op) => {
                    if PRECEDENCE[level].contains(op) {
                        let left_exp = match construct_exp_tree(left_side.iter().peekable()) {
                            Ok(exp) => exp,
                            Err(err) => return Err(err),
                        };

                        terms.next();
                        let right_exp = match construct_exp_tree(terms) {
                            Ok(exp) => {exp},
                            Err(err) => return Err(err),
                        };
                        
                        return Ok(Exp::BinOpExp(Box::new(left_exp), parse_operator(op), Box::new(right_exp), **loc))
                    } else {
                        left_side.push((term, *loc))
                    }
                },
                Term::ExpTerm(_) => left_side.push((term, *loc))
            }

            terms.next();
        }
    }

    unreachable!()
}

fn parse_operator(op: &LexToken) -> ast::Operator {
    match op {
        LexToken::Operator(op) => match *op {
            "+" => ast::Operator::Plus,
            "-" => ast::Operator::Minus,
            "*" => ast::Operator::Multiply,
            "/" => ast::Operator::Divide,
            "%" => ast::Operator::Modulo,
            "<" => ast::Operator::LessThan,
            ">" => ast::Operator::GreaterThan,
            "<=" => ast::Operator::LessOrEquals,
            ">=" => ast::Operator::GreaterOrEquals,
            
            _ => panic!("Unknown operator")
        },
        _ => panic!("Not an operator"),
    }
}

fn parse_simple(lexed: LexIter) -> ParseRes {
    parse_int(lexed)
    //todo!();
}

fn parse_if(lexed: LexIter) -> ParseRes {
    todo!();
}

fn parse_while(lexed: LexIter) -> ParseRes {
    todo!();
}

fn parse_for(lexed: LexIter) -> ParseRes {
    todo!();
}

fn parse_block(lexed: LexIter) -> ParseRes {
    todo!();
}

fn parse_int(lexed: LexIter) -> ParseRes {
    let mut lexed = lexed.clone();
    match lexed.peek() {
        Some((LexToken::Int(i), loc)) => {
            lexed.next();
            Ok((Exp::LiteralExp(Literal::Int(*i), *loc), lexed))
        },
        Some((token, loc)) => {
            lexed.next();
            Err((format!("Expected integer, got {token:?}"), *loc))
        },
        _ => Err((format!("Expected integer"), lexed.last().unwrap().1))
    }
}

fn parenthesis(lexed: LexIter, paren: char) -> Result<LexIter, (String, Location)> {
    let mut lexed = lexed.clone();
    match lexed.peek() {
        Some((Paren(paren), _)) => {
            lexed.next();
            Ok(lexed)
        },
        Some((_, loc)) => {
            Err((format!("Expected '{paren}'"), *loc))
        },
        _ => Err((format!("Expected '{paren}'"), lexed.last().unwrap().1))
    }
}