use core::panic;
use std::{iter::Peekable, slice::Iter};

use lazy_static::lazy_static;

use super::*;

use LexToken::*;

type LexIter<'a> = Peekable<Iter<'a, (LexToken, Location)>>;
type ParseRes<'a> = Result<(Exp, LexIter<'a>), (String, Location)>;

lazy_static!(
    ///All legal operators
    pub static ref OPERATORS: Vec<&'static str> = Vec::from(["+=", "-=", "+", "-", "*", "/", "%", "<=", ">=", "<", ">", "!=", "!", "==", "=", "&&", "||"]);

    ///All legal keywords
    pub static ref KEYWORDS: Vec<&'static str> = Vec::from(["if", "else", "while", "for", "let", "fun"]);

    pub static ref PRECEDENCE: Vec<Vec<LexToken>> = vec![
        //Unary
        vec![Operator("-"), Operator("!")],

        //Binary
        vec![Operator("*"), Operator("/"), Operator("%")],  
        vec![Operator("+"), Operator("-")],
        vec![Operator("<"), Operator(">"), Operator("<="), Operator(">="), Operator("==")],
        vec![Operator("=="), Operator("!=")],
        vec![Operator("&&")],
        vec![Operator("||")],
        vec![Operator("="), Operator("+="), Operator("-=")],
    ];

    pub static ref TERMINATORS: Vec<LexToken> = vec![
        SemiColon,
        Paren(')'),
        Paren('}'),
        Paren(']'),
        Keyword("else"),
        EndOfInput
    ];

    pub static ref VALUE_KEYWORDS: Vec<LexToken> = vec![
        Keyword("if"),
    ];
);

#[derive(Debug)]
enum Term<'a> {
    OpTerm(&'a LexToken),
    ExpTerm(Exp)
}

pub fn parse(lexed: LexIter) -> ParseRes {
    parse_block(lexed, false)
}

fn parse_exp(lexed: LexIter) -> ParseRes {
    let mut lexed = lexed.clone();
    let mut terms: Vec<(Term, Location)> = Vec::new();
    let loc = curr_loc(&lexed);

    while let Some(peek) = lexed.peek() {
        let token = &peek.0;
        let loc = &peek.1;

        if TERMINATORS.contains(token) {
            break;
        }

        match token {
            Keyword(_) => if terms.len() > 0 && !VALUE_KEYWORDS.contains(token) {
                break;
            },
            _ => {}
        }

        match token {
            Keyword(kwd) => {
                match match *kwd {
                    "if" => parse_if(lexed),
                    "while" => parse_while(lexed),
                    "for" => parse_for(lexed),
                    "let" => parse_let(lexed),
                    _ => Err((format!("Unknown keyword '{kwd}'"), *loc))
                } {
                    Ok((exp, rest)) => {
                        terms.push((Term::ExpTerm(exp), *loc));
                        lexed = rest;
                        if !VALUE_KEYWORDS.contains(token) {
                            break
                        }
                    },
                    Err(err) => return Err(err),
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
            Paren('{') => match parse_block(lexed, true) {
                Ok((exp, rest)) => {
                    terms.push((Term::ExpTerm(exp), *loc));
                    lexed = rest;
                },
                Err(err) => return Err(err)
            },
            LexToken::Operator(_) => {
                terms.push((Term::OpTerm(token), *loc));
                lexed.next();
            },
            Id(id) =>{
                //Check for fun call first TODO
                terms.push((Term::ExpTerm(Exp::VarExp(id.clone(), *loc)), *loc));
                lexed.next();
            },
            _ => match parse_simple(lexed) {
                Ok((exp, rest)) => {
                    terms.push((Term::ExpTerm(exp), *loc));
                    lexed = rest;
                },
                Err(err) => return Err(err),
            },
        };
    }

    //Make tree
    //Transform to references
    let mut transformed = Vec::new();
    for i in 0..terms.len() {
        transformed.push((&terms[i].0, &terms[i].1))
    }
    
    match construct_exp_tree(transformed.iter().peekable(), loc) {
        Ok(exp) => Ok((exp, lexed)),
        Err(err) => Err(err),
    }
}

fn construct_exp_tree<'a>(terms: Peekable<std::slice::Iter<'_, (&Term<'_>, &lexer::Location)>>, loc: Location) -> Result<Exp, (String, Location)> {
    let mut terms = terms.clone();

    //This is an atomic node
    if terms.len() == 1 {
        return match terms.next() {
            Some(term) => match term {
                (Term::OpTerm(op), loc) => Err((format!("Expected an expression, got {op:?}"), **loc)),
                (Term::ExpTerm(exp), _) => Ok(exp.clone()),
            },
            None => unreachable!(),
        }
    }

    //Precedence
    for level in (0..PRECEDENCE.len()).rev() {
        let mut terms = terms.clone();
        let mut left_side: Vec<(&Term, &Location)> = Vec::new();
        while let Some(entry) = terms.peek() {
            let term = &entry.0;
            let loc = &entry.1;
            
            match term {
                Term::OpTerm(op) => {
                    if PRECEDENCE[level].contains(*op) {
                        //If there is no left side or not a left side with immediate symbol, it is not a unary op
                        if level > 0 && left_side.len() > 0 && match left_side.last().unwrap().0 {
                            Term::OpTerm(_) => false,
                            Term::ExpTerm(_) => true,
                        } {
                            let left_exp = match construct_exp_tree(left_side.iter().peekable(), **loc) {
                                Ok(exp) => exp,
                                Err(err) => return Err(err),
                            };
                            terms.next();
                            let right_exp = match construct_exp_tree(terms, **loc) {
                                Ok(exp) => exp,
                                Err(err) => return Err(err),
                            };

                            return Ok(Exp::BinOpExp(Box::new(left_exp), parse_operator(op), Box::new(right_exp), **loc))
                        } else if level == 0 && left_side.len() == 0 {
                            terms.next();
                            let right_exp = match construct_exp_tree(terms, **loc) {
                                Ok(exp) => exp,
                                Err(err) => return Err(err),
                            };
                            
                            return Ok(Exp::UnOpExp(parse_operator(op), Box::new(right_exp), **loc))
                        }
                        else {
                            left_side.push((term, *loc))
                        }
                    } else {
                        left_side.push((term, *loc))
                    }
                },
                Term::ExpTerm(_) => left_side.push((term, *loc))
            }

            terms.next();
        }
    }

    //Illegal syntax
    Err((format!("Illegal syntax: Invalid expression: {terms:?}"), loc))
}

fn curr_loc(lexed: &LexIter) -> Location {
    let mut lexed = lexed.clone();
    match lexed.peek() {
        Some((_, loc)) => *loc,
        None => panic!("Could not get location"),
    }
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
            "!" => ast::Operator::Not,
            "==" => ast::Operator::Equals,
            "=" => ast::Operator::Assign,
            "+=" => ast::Operator::PlusAssign,
            "-=" => ast::Operator::MinusAssign,
            "&&" => ast::Operator::And,
            "||" => ast::Operator::Or,
            "!=" => ast::Operator::NotEquals,
            
            _ => panic!("Unknown operator")
        },
        _ => panic!("Not a legal operator"),
    }
}

fn parse_simple(lexed: LexIter) -> ParseRes {
    let mut lexed = lexed.clone();
    let result = if let Some(token) = lexed.peek() {
        let loc = token.1;
        match token.0 {
            LexToken::Int(i) => Exp::LiteralExp(Literal::Int(i), loc),
            LexToken::Float(f) => Exp::LiteralExp(Literal::Float(f), loc),
            LexToken::Bool(b) => Exp::LiteralExp(Literal::Bool(b), loc),
            _ => return Err((format!("Expected simple token, but got {token:?}"), loc))
        }
    } else { unreachable!() };

    lexed.next();

    Ok((result, lexed))
}

fn parse_if(lexed: LexIter) -> ParseRes {
    let mut lexed = lexed.clone();
    
    //Get rid of keyword
    let loc = match lexed.peek() {
        Some((Keyword("if"), loc)) => loc,
        _ => panic!("There should be an if here"),
    };

    lexed.next();

    match parenthesis(lexed, '(') {
        Ok(rest) => lexed = rest,
        Err(err) => return Err(err),
    }

    let cond = match parse_exp(lexed) {
        Ok((exp, rest)) => {
            lexed = rest;
            exp
        },
        Err(err) => return Err(err),
    };

    match parenthesis(lexed, ')') {
        Ok(rest) => lexed = rest,
        Err(err) => return Err(err),
    }

    let pos = match parse_exp(lexed) {
        Ok((exp, rest)) => {
            lexed = rest;
            exp
        },
        Err(err) => return Err(err),
    };

    match lexed.peek() {
        Some((Keyword("else"), _)) => { lexed.next(); },
        _ => return Ok((Exp::IfElseExp(Box::new(cond), Box::new(pos), None, *loc), lexed))
    }

    let neg = match parse_exp(lexed) {
        Ok((exp, rest)) => {
            lexed = rest;
            exp
        },
        Err(err) => return Err(err),
    };
    
    Ok((Exp::IfElseExp(Box::new(cond), Box::new(pos), Some(Box::new(neg)), *loc), lexed))
}

fn parse_id(lexed: LexIter) -> Result<&String, String> {
    let mut lexed = lexed.clone();
    let result = match lexed.next() {
        Some((LexToken::Id(id), _)) => Ok(id),
        _ => Err(format!("Not an id")),
    };

    lexed.next();

    result
}

fn parse_let(lexed: LexIter) -> ParseRes {
    let mut lexed = lexed.clone();
    
    //Get rid of keyword
    let loc = match lexed.peek() {
        Some((Keyword("let"), loc)) => loc,
        _ => panic!("There should be a let here"),
    };

    lexed.next();

    let id = match parse_id(lexed.clone()) {
        Ok(id) => id,
        Err(err) => return Err((err, curr_loc(&lexed))),
    };

    lexed.next();

    match lexed.peek() {
        Some((LexToken::Operator("="), _)) => lexed.next(),
        _ => return Err((format!("Illegal 'let' statement"), *loc)),
    };

    let exp = match parse_exp(lexed) {
        Ok((exp, rest)) => {
            lexed = rest;
            exp
        },
        Err(err) => return Err(err),
    };

    Ok((Exp::LetExp(id.clone(), Box::new(exp), *loc), lexed))
}

fn parse_while(lexed: LexIter) -> ParseRes {
    let mut lexed = lexed.clone();
    
    //Get rid of keyword
    let loc = match lexed.peek() {
        Some((Keyword("while"), loc)) => loc,
        _ => panic!("There should be a while here"),
    };

    lexed.next();

    match parenthesis(lexed, '(') {
        Ok(rest) => lexed = rest,
        Err(err) => return Err(err),
    }

    let cond = match parse_exp(lexed) {
        Ok((exp, rest)) => {
            lexed = rest;
            exp
        },
        Err(err) => return Err(err),
    };

    match parenthesis(lexed, ')') {
        Ok(rest) => lexed = rest,
        Err(err) => return Err(err),
    }

    let exp = match parse_block(lexed, true) {
        Ok((exp, rest)) => {
            lexed = rest;
            exp
        },
        Err(err) => return Err(err),
    };

    Ok((Exp::WhileExp(Box::new(cond), Box::new(exp), *loc), lexed))
}

fn parse_for(lexed: LexIter) -> ParseRes {
    todo!();
}

fn parse_block(lexed: LexIter, curly: bool) -> ParseRes {
    let mut lexed = lexed.clone();
    let loc = curr_loc(&lexed);

    if curly {
        match parenthesis(lexed, '{') {
            Ok(rest) => {
                lexed = rest
            },
            Err(err) => return Err(err),
        };
    }

    let mut exps: Vec<Exp> = Vec::new();

    while let Some(token) = lexed.peek() {
        if curly {
            match token {
                (LexToken::Paren('}'), _) => {
                    lexed.next();
                    return Ok((Exp::BlockExp(exps, loc), lexed));
                },
                _ => {}
            }
        }

        match token {
            (SemiColon, _) => { lexed.next(); },
            (EndOfInput, _) => { break },
            _ => match parse_exp(lexed) {
                Ok((exp, rest)) => {
                    lexed = rest;
                    exps.push(exp)
                },
                Err(err) => return Err(err),
            },
        };
    }

    if curly {
        Err((format!("Unmatched '{{'"), loc))
    } else {
        Ok((Exp::BlockExp(exps, loc), lexed))
    }
    
}

fn parenthesis(lexed: LexIter, paren: char) -> Result<LexIter, (String, Location)> {
    let mut lexed = lexed.clone();
    match lexed.peek() {
        Some((Paren(par), loc)) => {
            if *par != paren {
                return Err((format!("Expected '{paren}'"), *loc))
            }
            lexed.next();
            Ok(lexed)
        },
        _ => Err((format!("Expected '{paren}'"), curr_loc(&lexed)))
    }
}