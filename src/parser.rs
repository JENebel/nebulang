use std::{iter::Peekable, slice::Iter};

use lazy_static::lazy_static;

use super::*;

use LexToken::*;
use ast::Operator::*;

type LexIter<'a> = Peekable<Iter<'a, (LexToken, Location)>>;
type KeepRes = Result<Exp, (String, Location)>;
type DiscardRes = Result<(), (String, Location)>;

lazy_static!(
    ///All legal operators
    pub static ref OPERATORS: Vec<&'static str> = Vec::from(["+=", "-=", "+", "-", "*", "/", "%", "<=", ">=", "<", ">", "!=", "!", "==", "=", "&&", "||"]);

    ///All legal keywords
    pub static ref KEYWORDS: Vec<&'static str> = Vec::from(["if", "else", "while", "for", "let", "fun"]);

    ///All legal types
    pub static ref TYPES: Vec<&'static str> = Vec::from(["int", "float", "bool", "unit"]);

    pub static ref UNARY_OPERATORS: Vec<ast::Operator> = vec![
        Minus,
        Not
    ];

    //Precedence of binary operators
    pub static ref BINARY_OP_PRECEDENCE: Vec<Vec<ast::Operator>> = vec![
        //Binary
        vec![Multiply, Divide, Modulo],  
        vec![Plus, Minus],
        vec![LessThan, GreaterThan, LessOrEquals, GreaterOrEquals],
        vec![Equals, NotEquals],
        vec![And],
        vec![Or],
        vec![Assign, PlusAssign, MinusAssign],
    ];

    pub static ref TERMINATORS: Vec<LexToken> = vec![
        SemiColon,
        Paren(')'),
        Paren('}'),
        Paren(']'),
        Keyword("else"),
        Comma,
        EndOfInput
    ];
);

pub fn statement(lexed: &mut LexIter) -> KeepRes {
    if let Some((token, _)) = lexed.peek() {
        return match token {
            //Fun decls are handled in: parse_statements()
            Paren('{') =>        block(lexed),
            Keyword("while") =>  wwhile(lexed),
            Keyword("let") =>    llet(lexed),
            _ => expression(lexed)
        }
    }

    unreachable!()
}

pub fn term(lexed: &mut LexIter) -> KeepRes {
    if let Some((token, _)) = lexed.peek() {
        return match token {
            Paren('{') =>                   block(lexed),
            Keyword("if") =>                iif(lexed),
            Paren('(') =>                   parenthesized_exp(lexed),
            Int(_) | Float(_) | Bool(_) =>  literal(lexed),
            Id(_) =>                        var_or_fun_call(lexed),

            _ => Err((format!("Expected a term"), curr_loc(lexed)?))
        };
    }

    unreachable!();
}

pub fn parse(lexed: &mut LexIter) -> KeepRes {
    parse_statements(lexed)
}

fn parse_statements(lexed: &mut LexIter) -> KeepRes {
    let loc = curr_loc(lexed)?;

    let mut exps: Vec<Exp> = Vec::new();
    let mut funs: Vec<(String, Box<Function>)> = Vec::new();

    while !terminator(lexed) {
        match lexed.peek() {
            Some((Keyword("fun"), _)) => {
                funs.push(fun_decl(lexed)?)
            },
            _ => exps.push(statement(lexed)?)
        }
        
        match semi_colon(lexed) { _ => {} } //Just discard the semicolon if it is present
    }

    Ok(Exp::BlockExp(exps, funs, loc))
}

#[derive(Clone, Debug)]
enum Term {
    ExpTerm(Exp),
    OpTerm(ast::Operator, Location)
}

fn expression(lexed: &mut LexIter) -> KeepRes {
    let mut terms: Vec<Term> = Vec::new();

    //Collect terms
    while !terminator(lexed) {
        if let Some((Operator(_), loc)) = lexed.peek() {
            terms.push(Term::OpTerm(any_operator(lexed)?, *loc))
        } else {
            if let Some(Term::ExpTerm(_)) = terms.last() {
                if terms.len() > 0 {
                    return Err((format!("Expected operator or ';'"), curr_loc(lexed)?))
                }
            }
    
            terms.push(Term::ExpTerm(term(lexed)?))
        }
    }

    if terms.len() == 0 {
        return Err((format!("Expected something"), curr_loc(lexed)?));
    }

    //Establish precedence
    Ok(precedence(terms.as_slice())?)
}

fn precedence(terms: &[Term]) -> KeepRes {
    //There should not be an operator last
    if let Some(Term::OpTerm(op, loc)) = terms.last() {
        return Err((format!("Unexpected operator '{op}'"), *loc))
    }

    if terms.len() == 1 {
        if let Some(Term::ExpTerm(exp)) = terms.first() {
            return Ok(exp.clone());
        }
    }

    //Binary operators
    for operators in BINARY_OP_PRECEDENCE.iter().rev() {
        let mut iter = terms.iter().enumerate();

        while let Some(term) = iter.next() {
            if let (i, Term::OpTerm(op, loc)) = term {
                if operators.contains(op) && i != 0 {
                    let split = terms.split_at(i);
                    return Ok(Exp::BinOpExp(Box::new(precedence(split.0)?), *op, Box::new(precedence(&split.1[1..])?), *loc));
                }
            }
        }
    }

    //Unary operators
    if let Some(Term::OpTerm(op, loc)) = terms.first() {
        if !UNARY_OPERATORS.contains(op) {
            return Err((format!("Not a unary operator '{op}'"), *loc))
        }
        return Ok(Exp::UnOpExp(*op, Box::new(precedence(&terms[1..])?), *loc));
    } else if let Some(Term::ExpTerm(_)) = terms.first() {
        if let Term::OpTerm(op, loc) = terms[1] {
            return Err((format!("Not a binary operator '{op}'"), loc))
        }
    }

    unreachable!()
}

fn iif(lexed: &mut LexIter) -> KeepRes {
    let loc = curr_loc(lexed)?;
    
    keyword(lexed, "if")?;
    let cond = parenthesized_exp(lexed)?;
    let pos = statement(lexed)?;

    match keyword(lexed, "else") {
        Ok(_) => {
            let neg = expression(lexed)?;
            Ok(Exp::IfElseExp(Box::new(cond), Box::new(pos), Some(Box::new(neg)), loc))
        },
        Err(_) => Ok(Exp::IfElseExp(Box::new(cond), Box::new(pos), None, loc))
    }
}

fn llet(lexed: &mut LexIter) -> KeepRes {
    let loc = curr_loc(lexed)?;

    keyword(lexed, "let")?;
    let id = id(lexed)?;
    operator(lexed, Assign)?;
    let exp = expression(lexed)?;

    Ok(Exp::LetExp(id.clone(), Box::new(exp), loc))
}

fn wwhile(lexed: &mut LexIter) -> KeepRes {
    let loc = curr_loc(lexed)?;

    keyword(lexed, "while")?;
    let cond = parenthesized_exp(lexed)?;
    let exp = statement(lexed)?;

    Ok(Exp::WhileExp(Box::new(cond), Box::new(exp), loc))
}

/*fn ffor(lexed: LexIter) -> KeepRes {
    todo!();
}*/

fn block(lexed: &mut LexIter) -> KeepRes {
    parenthesis(lexed, '{')?;
    let block = parse_statements(lexed)?;
    parenthesis(lexed, '}')?;
    Ok(block)
}

fn parenthesized_exp(lexed: &mut LexIter) -> KeepRes {
    parenthesis(lexed, '(')?;
    let exp = expression(lexed)?;
    parenthesis(lexed, ')')?;
    Ok(exp)
}

fn any_operator(lexed: &mut LexIter) -> Result<ast::Operator, (String, Location)> {
    match lexed.peek() {
        Some((LexToken::Operator(op), loc)) => {
            let res = match *op {
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
                _ => return Err((format!("Unknown operator: '{op}"), *loc))
            };
            lexed.next();
            Ok(res)
        },
        _ => Err((format!("Expected an operator"), curr_loc(lexed)?))
    }
}

fn operator(lexed: &mut LexIter, operator: ast::Operator) -> Result<ast::Operator, (String, Location)> {
    match any_operator(lexed) {
        Ok(actual) => if actual == operator {
            Ok(actual)
        } else {
            Err((format!("Expected '{operator}', got {actual}"), curr_loc(lexed)?))
        },
        Err(_) => Err((format!("Expected '{operator}'"), curr_loc(lexed)?)),
    }
}

fn keyword(lexed: &mut LexIter, keyword: &str) -> DiscardRes {
    match lexed.peek() {
        Some((Keyword(kwd), _)) => if *kwd == keyword {
            lexed.next();
            Ok(())
        } else {
            Err((format!(""), curr_loc(lexed)?))
        },
        _ => Err((format!("Expected {keyword}"), curr_loc(lexed)?))
    }
}

fn id(lexed: &mut LexIter) -> Result<String, (String, Location)> {
    match lexed.peek() {
        Some((LexToken::Id(id), _)) => {
            lexed.next();
            Ok(id.clone())
        },
        _ => Err((format!("Expected an identifier"), curr_loc(lexed)?)),
    }
}

fn var_or_fun_call(lexed: &mut LexIter) -> KeepRes {
    let loc = curr_loc(lexed)?;
    let id = id(lexed)?;

    match lexed.peek() {
        Some((Paren('('), _)) => {
            parenthesis(lexed, '(')?;

            let mut params = Vec::new();
            loop {
                if terminator(lexed) {
                    break
                }
                let param = expression(lexed)?;
                params.push(param);
                let _ = comma(lexed);
            }
            parenthesis(lexed, ')')?;

            Ok(Exp::FunCallExp(id, params, loc))
        },
        _ => Ok(Exp::VarExp(id, loc)),
    }
}

fn fun_decl(lexed: &mut LexIter) -> Result<(String, Box<Function>), (String, Location)> {
    let loc = curr_loc(lexed)?;
    keyword(lexed, "fun")?;
    let name = id(lexed)?;
    parenthesis(lexed, '(')?;
    let mut params = Vec::new();
    let mut p_types = Vec::new();
    while let Ok(param) = id(lexed) {
        params.push(param);
        colon(lexed)?;
        p_types.push(any_type(lexed)?);

        if let Err(_) = comma(lexed) {
            break
        }
    }
    parenthesis(lexed, ')')?;

    let return_type = if let Ok(_) = colon(lexed) {
        any_type(lexed)?
    } else {
        ast::Type::Any
    };

    operator(lexed, Assign)?;

    let exp = statement(lexed)?;

    let func = Function {
        ret_type: return_type,
        param_types: p_types,
        params,
        exp: Box::new(exp),
        loc
    };

    Ok((name, Box::new(func)))
}

fn any_type(lexed: &mut LexIter) -> Result<ast::Type, (String, Location)> {
    match lexed.peek() {
        Some((Type(typ), loc)) => {
            let typ = match *typ {
                "int" => ast::Type::Int,
                "float" => ast::Type::Float,
                "bool" => ast::Type::Bool,
                "unit" => ast::Type::Unit,

                _ => return Err((format!("Unknown type"), *loc))
            };
            lexed.next();
            return Ok(typ);
        } 
        _ => Err((format!("Expected a type"), curr_loc(lexed)?))
    }
}

fn literal(lexed: &mut LexIter) -> KeepRes {
    let loc = curr_loc(lexed)?;

    let lit = match lexed.peek() {
            Some((LexToken::Int(i), _)) => Exp::LiteralExp(Literal::Int(*i), loc),
            Some((LexToken::Float(f), _)) => Exp::LiteralExp(Literal::Float(*f), loc),
            Some((LexToken::Bool(b), _)) => Exp::LiteralExp(Literal::Bool(*b), loc),
            Some((_, _))=> return Err((format!("Expected literal"), loc)),
            _ => unreachable!()
    };
    lexed.next();
    Ok(lit)
}

fn semi_colon(lexed: &mut LexIter) -> DiscardRes {
    match lexed.peek() {
        Some((SemiColon, _)) => {
            lexed.next();
            Ok(())
        },
        _ => Err((format!("Expected ';'"), curr_loc(lexed)?)),
    }
}

fn colon(lexed: &mut LexIter) -> DiscardRes {
    match lexed.peek() {
        Some((Colon, _)) => {
            lexed.next();
            Ok(())
        },
        _ => Err((format!("Expected ':'"), curr_loc(lexed)?)),
    }
}

fn comma(lexed: &mut LexIter) -> DiscardRes {
    match lexed.peek() {
        Some((Comma, _)) => {
            lexed.next();
            Ok(())
        },
        _ => Err((format!("Expected ','"), curr_loc(lexed)?)),
    }
}

fn parenthesis(lexed: &mut LexIter, paren: char) -> Result<(), (String, Location)> {
    match lexed.peek() {
        Some((Paren(par), loc)) => {
            if *par != paren {
                return Err((format!("Expected '{paren}'"), *loc))
            }
            lexed.next();
            Ok(())
        },
        _ => Err((format!("Expected '{paren}'"), curr_loc(lexed)?))
    }
}

fn terminator(lexed: &mut LexIter) -> bool {
    TERMINATORS.contains(&lexed.peek().unwrap().0)
}

fn curr_loc(lexed: &mut LexIter) -> Result<Location, (String, Location)> {
    match lexed.peek() {
        Some((EndOfInput, loc)) => Err((format!("Unexpected end of input"), *loc)),
        Some((_, loc)) => Ok(*loc),
        None => panic!("Could not get location"),
    }
}