use std::{iter::Peekable, slice::Iter, cell::RefCell, rc::Rc};
use lazy_static::lazy_static;

use crate::ast::*;
use crate::lexer::*;
use crate::environment::*;
use crate::ast as ast;

use LexToken::*;
use ast::Operator::*;

type LexIter<'a> = Peekable<Iter<'a, (LexToken, Location)>>;
type KeepRes = Result<Exp, Error>;
type DiscardRes = Result<(), Error>;

#[derive(Clone, Debug)]
enum Term {
    ExpTerm(Exp),
    OpTerm(ast::Operator, Location)
}

lazy_static!(//                                                  for
    ///All legal operators                                   [ comments ]
    pub static ref OPERATORS: Vec<&'static str> = Vec::from([ "//", "/*", "+=", "-=", "+", "-", "*", "/", "%", "<=", ">=", "<", ">", "!=", "!", "==", "=", "&&", "||"]);

    ///All legal keywords
    pub static ref KEYWORDS: Vec<&'static str> = Vec::from(["if", "else", "while", "for", "let", "fun", "of", "return", "break", "continue", "yield"]);

    ///All legal types
    pub static ref TYPES: Vec<&'static str> = Vec::from(["int", "float", "bool", "char", "string", "unit"]);

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
        Keyword("of"),
        Comma,
        EndOfInput
    ];

    /// These tokens are strictly used WITHIN other statements
    pub static ref WITHIN_TOKENS: Vec<LexToken> = vec![
        Keyword("else"),
        Keyword("of"),
    ];
);

/// Parses a program to an AST representation
/// 
/// Arguments
/// * `lexed` - The lexed program
pub fn parse(lexed: &mut LexIter) -> Result<(Exp, FunStore), Error> {
    let mut fun_store = Rc::new(RefCell::new(Vec::new()));
    let res = parse_statements(lexed, &mut fun_store)?;
    Ok((res, fun_store))
}

/// Parses a statement. Loops, block, let etc.
fn parse_statement(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    if within_token(lexed) {
        return Err(Error::new(ErrorType::SyntaxError, format!("Unexpected token."), curr_loc(lexed)?))
    }

    if let Some((token, _)) = lexed.peek() {
        return match token {
            //Fun decls are handled in: parse_statements()
            Paren('{') =>        parse_block(lexed, fun_store),
            Keyword("while") =>  parse_while(lexed, fun_store),
            Keyword("for") =>    parse_for(lexed, fun_store),
            Keyword("let") =>    parse_let(lexed, fun_store),
            Keyword("if") =>     parse_if(lexed, fun_store),
            Keyword("return") =>     parse_return(lexed, fun_store),
            _ => parse_expression(lexed, fun_store)
        }
    }

    unreachable!()
}

/// Parses a term to be used in an expression
fn parse_term(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    if let Some((token, _)) = lexed.peek() {
        return match token {
            Paren('{') =>                   parse_block(lexed, fun_store),
            Keyword("if") =>                parse_if(lexed, fun_store),
            Paren('(') =>                   parse_parenthesized_exp(lexed, fun_store),
            Paren('[') =>                   parse_array_init_or_acces(lexed, fun_store),
            Int(_) | Float(_) | Bool(_)
            | Char(_) | Str(_) =>           parse_literal(lexed),
            Id(_) =>                        parse_var_or_fun_call(lexed, fun_store),

            _ => Err(Error::new(ErrorType::SyntaxError, format!("Expected a term."), curr_loc(lexed)?))
        };
    }

    unreachable!();
}

/// Parses a list of statements to a BlockExp
fn parse_statements(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let loc = curr_loc(lexed)?;

    let mut exps: Vec<Exp> = Vec::new();
    let mut funs: Vec<(String, usize)> = Vec::new();

    while !terminator(lexed) || within_token(lexed) {
        match lexed.peek() {
            Some((Keyword("fun"), _)) => {
                let decl = parse_fun_decl(lexed, fun_store)?;
                exps.push(decl.0);
                funs.push((decl.1, decl.2));
            },
            _ => exps.push(parse_statement(lexed, fun_store)?)
        }
        
        let _ = parse_semi_colon(lexed); //Just discard the semicolon if it is present
    }

    Ok(Exp::BlockExp(exps, funs, loc))
}

/// Parses an expression
fn parse_expression(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let mut terms: Vec<Term> = Vec::new();

    // Collect terms
    while !terminator(lexed) {
        if let Some((Operator(_), loc)) = lexed.peek() {
            terms.push(Term::OpTerm(parse_any_operator(lexed)?, *loc))
        } else {
            match parse_term(lexed, fun_store)? {
                Exp::AccessArrayExp(_, index_exp, loc) => {
                    // Combining the last expression with the access expression
                    if let Term::ExpTerm(exp) = terms[terms.len() - 1].clone() {
                        // Array access
                        let arr_exp = exp;
                        terms.remove(terms.len() - 1);
                        terms.push(Term::ExpTerm(Exp::AccessArrayExp(Box::new(arr_exp), Box::new(*index_exp), loc)))
                    } else {
                        return Err(Error::new(ErrorType::SyntaxError, format!("Incorrect array access syntax."), curr_loc(lexed)?))
                    }
                },
                exp => {
                    if let Some(Term::ExpTerm(_)) = terms.last() {
                        if terms.len() > 0 {
                            return Err(Error::new(ErrorType::SyntaxError, format!("Expected operator or ';'."), curr_loc(lexed)?))
                        }
                    }
                    terms.push(Term::ExpTerm(exp))
                }
            }
        }
    }

    if terms.len() == 0 {
        return Err(Error::new(ErrorType::SyntaxError, format!("Expected expression. Got nothing."), curr_loc(lexed)?))
    }

    //Establish precedence
    Ok(establish_precedence(terms.as_slice())?)
}

/// Establishes precedence in a list of terms
fn establish_precedence(terms: &[Term]) -> KeepRes {
    //There should not be an operator last
    if let Some(Term::OpTerm(op, loc)) = terms.last() {
        return Err(Error::new(ErrorType::SyntaxError, format!("Unexpected operator '{op}'."), *loc))
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
                    return Ok(Exp::BinOpExp(
                        Box::new(establish_precedence(split.0)?), 
                        *op, 
                        Box::new(establish_precedence(&split.1[1..])?), *loc)
                    );
                }
            }
        }
    }

    //Unary operators
    if let Some(Term::OpTerm(op, loc)) = terms.first() {
        if !UNARY_OPERATORS.contains(op) {
            return Err(Error::new(ErrorType::SyntaxError, format!("Not a unary operator '{op}'."), *loc))
        }
        return Ok(Exp::UnOpExp(*op, Box::new(establish_precedence(&terms[1..])?), *loc));
    } else if let Some(Term::ExpTerm(_)) = terms.first() {
        if let Term::OpTerm(op, loc) = terms[1] {
            return Err(Error::new(ErrorType::SyntaxError, format!("Not a binary operator '{op}'."), loc))
        }
    }

    unreachable!()
}

/// Parses an if statement/term
fn parse_if(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let loc = curr_loc(lexed)?;
    
    parse_keyword(lexed, "if")?;
    let cond = parse_parenthesized_exp(lexed, fun_store)?;
    let pos = parse_statement(lexed, fun_store)?;

    let _ = parse_semi_colon(lexed);

    match parse_keyword(lexed, "else") {
        Ok(_) => {
            let neg = parse_statement(lexed, fun_store)?;
            Ok(Exp::IfElseExp(Box::new(cond), Box::new(pos), Some(Box::new(neg)), loc))
        },
        Err(_) => Ok(Exp::IfElseExp(Box::new(cond), Box::new(pos), None, loc))
    }
}

/// Parses a let expression
fn parse_let(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let loc = curr_loc(lexed)?;

    parse_keyword(lexed, "let")?;
    let id = parse_id(lexed)?;
    parse_operator(lexed, Assign)?;
    let exp = parse_expression(lexed, fun_store)?;

    Ok(Exp::LetExp(id.clone(), Box::new(exp), loc))
}

fn parse_return(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let loc = curr_loc(lexed)?;

    parse_keyword(lexed, "return")?;

    if terminator(lexed) {
        // Assumes unit if no expression is given
        return Ok(Exp::ReturnExp(Box::new(Exp::LiteralExp(Literal::Unit, loc)), loc))
    }

    let value_exp = parse_expression(lexed, fun_store)?;
    
    Ok(Exp::ReturnExp(Box::new(value_exp), loc))
}

/// Parses a while loop
fn parse_while(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let loc = curr_loc(lexed)?;

    parse_keyword(lexed, "while")?;
    let cond = parse_parenthesized_exp(lexed, fun_store)?;
    let exp = parse_statement(lexed, fun_store)?;

    Ok(Exp::WhileExp(Box::new(cond), Box::new(exp), loc))
}

/// Parses a for loop
fn parse_for(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let loc = curr_loc(lexed)?;

    parse_keyword(lexed, "for")?;
    parse_parenthesis(lexed, '(')?;
    if let Ok(id) = parse_id(lexed) {
        // Normal for loop
        parse_operator(lexed, Assign)?;

        let let_loc = curr_loc(lexed)?;
        let from_exp = parse_expression(lexed, fun_store)?;
        let let_exp = Box::new(Exp::LetExp(id.clone(), Box::new(from_exp), let_loc));

        parse_semi_colon(lexed)?;

        let cond_exp = Box::new(parse_expression(lexed, fun_store)?);
        
        parse_semi_colon(lexed)?;

        let inc_exp = Box::new(parse_expression(lexed, fun_store)?);

        parse_parenthesis(lexed, ')')?;

        let body = Box::new(parse_statement(lexed, fun_store)?);

        Ok(Exp::ForExp(
            let_exp,
            cond_exp,
            inc_exp,
            body,
            loc
        ))
    } else {
        // Simple for loop
        let id = format!(".for");
        let let_exp = Box::new(Exp::LetExp(id.clone(), Box::new(Exp::LiteralExp(Literal::Int(0), loc)), loc));
        let cond = Box::new(Exp::BinOpExp(Box::new(Exp::VarExp(id.clone(), loc)), ast::Operator::LessThan, Box::new(parse_expression(lexed, fun_store)?), loc));
        let increment = Box::new(Exp::BinOpExp(Box::new(Exp::VarExp(id, loc)), ast::Operator::PlusAssign, Box::new(Exp::LiteralExp(Literal::Int(1), loc)), loc));
        
        parse_parenthesis(lexed, ')')?;

        let body = Box::new(parse_statement(lexed, fun_store)?);

        Ok(Exp::ForExp(
            let_exp,
            cond,
            increment,
            body,
            loc
        ))
    }
}

/// Parses a block. Eg. a series of statements statements surrounded by { }
fn parse_block(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    parse_parenthesis(lexed, '{')?;
    let block = parse_statements(lexed, fun_store)?;
    parse_parenthesis(lexed, '}')?;
    Ok(block)
}

/// Parses an exp surrounded by parentheses
fn parse_parenthesized_exp(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    parse_parenthesis(lexed, '(')?;
    let exp = parse_expression(lexed, fun_store)?;
    parse_parenthesis(lexed, ')')?;
    Ok(exp)
}

/// Parses any operator
fn parse_any_operator(lexed: &mut LexIter) -> Result<ast::Operator, Error> {
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
                _ => return Err(Error::new(ErrorType::SyntaxError, format!("Unknown operator: '{op}'."), *loc))
            };
            lexed.next();
            Ok(res)
        },
        _ => return Err(Error::new(ErrorType::SyntaxError, format!("Expected an operator."), curr_loc(lexed)?))
    }
}

/// Parses a specific operator
fn parse_operator(lexed: &mut LexIter, operator: ast::Operator) -> Result<ast::Operator, Error> {
    match parse_any_operator(lexed) {
        Ok(actual) => if actual == operator {
            Ok(actual)
        } else {
            Err(Error::new(ErrorType::SyntaxError, format!("Expected '{operator}', got '{actual}'."), curr_loc(lexed)?))
        },
        Err(_) => Err(Error::new(ErrorType::SyntaxError, format!("Expected '{operator}'."), curr_loc(lexed)?))
    }
}

/// Parses a specific keyword
fn parse_keyword(lexed: &mut LexIter, keyword: &str) -> DiscardRes {
    match lexed.peek() {
        Some((Keyword(kwd), _)) => if *kwd == keyword {
            lexed.next();
            Ok(())
        } else {
            Err(Error::new(ErrorType::SyntaxError, format!("Expected keyword '{keyword}', but go keyword '{kwd}'."), curr_loc(lexed)?))
        },
        _ => Err(Error::new(ErrorType::SyntaxError, format!("Expected keyword '{keyword}'."), curr_loc(lexed)?))
    }
}

/// Parses an identifier
fn parse_id(lexed: &mut LexIter) -> Result<String, Error> {
    match lexed.peek() {
        Some((LexToken::Id(id), _)) => {
            lexed.next();
            Ok(id.clone())
        },
        _ => return Err(Error::new(ErrorType::SyntaxError, format!("Expected an identifier."), curr_loc(lexed)?)),
    }
}

/// Parses a term that starts with an id. This means a VarExp or FunCallExp
fn parse_var_or_fun_call(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let loc = curr_loc(lexed)?;
    let id = parse_id(lexed)?;

    if let Some((Paren('('), _)) = lexed.peek() {
        // Function call

        parse_parenthesis(lexed, '(')?;

        let mut params = Vec::new();
        loop {
            if terminator(lexed) {
                break
            }
            params.push(parse_expression(lexed, fun_store)?);
            let _ = parse_comma(lexed);
        }
        parse_parenthesis(lexed, ')')?;

        return Ok(Exp::FunCallExp(id, params, loc))
    }

    Ok(Exp::VarExp(id, loc))
}

/// Parses a function declaration.
/// 
/// On succes, returns a tuple with the (FunDeclExp, fun id, store index)
fn parse_fun_decl(lexed: &mut LexIter, fun_store: &mut FunStore) -> Result<(Exp, String, usize), Error> {
    let loc = curr_loc(lexed)?;
    parse_keyword(lexed, "fun")?;
    let name = parse_id(lexed)?;
    parse_parenthesis(lexed, '(')?;
    let mut params = Vec::new();
    let mut p_types = Vec::new();
    while let Ok(param) = parse_id(lexed) {
        params.push(param);
        parse_colon(lexed)?;
        p_types.push(parse_any_type(lexed)?);

        if let Err(_) = parse_comma(lexed) {
            break
        }
    }
    parse_parenthesis(lexed, ')')?;

    let return_type = if let Ok(_) = parse_colon(lexed) {
        parse_any_type(lexed)?
    } else {
        ast::Type::Any
    };

    parse_operator(lexed, Assign)?;

    let exp = parse_statement(lexed, fun_store)?;

    let func = Function {
        annotated: return_type != ast::Type::Any,
        ret_type: return_type,
        param_types: p_types,
        params,
        exp: Box::new(exp),
        loc
    };

    let store_index = fun_store.borrow().len();
    fun_store.borrow_mut().push(Rc::new(RefCell::new(Box::new(func))));

    Ok((Exp::FunDeclExp(name.clone(), loc), name, store_index))
}

fn parse_array_init_or_acces(lexed: &mut LexIter, fun_store: &mut FunStore) -> KeepRes {
    let loc = curr_loc(lexed)?;
    parse_parenthesis(lexed, '[')?;
    let length_exp = parse_expression(lexed, fun_store)?;

    if let Ok(_) = parse_parenthesis(lexed, ']') {
        // Array access
        // The Unit LiteralExp is just a placeholder and is replaced in the parse_expression function

        return Ok(Exp::AccessArrayExp(Box::new(Exp::LiteralExp(Literal::Unit, loc)), Box::new(length_exp), loc))
    }

    if let Ok(_) = parse_comma(lexed) {
        // Array init with initial values

        let mut values = Vec::new();
        values.push(length_exp); // The first value
        loop {
            if terminator(lexed) {
                break
            }
            values.push(parse_expression(lexed, fun_store)?);
            let _ = parse_comma(lexed);
        }
        parse_parenthesis(lexed, ']')?;

        return Ok(Exp::InitArrayWithValuesExp(values, loc))
    }

    parse_keyword(lexed, "of")?;
    let template_exp = parse_expression(lexed, fun_store)?;
    parse_parenthesis(lexed, ']')?;

    Ok(Exp::InitTemplateArrayExp(Box::new(length_exp), Box::new(template_exp), loc))
}

/// Parses next token and converts to an ast-type
fn parse_any_type(lexed: &mut LexIter) -> Result<ast::Type, Error> {
    match lexed.peek() {
        Some((Type(typ), loc)) => {
            let typ = match *typ {
                "int" => ast::Type::Int,
                "float" => ast::Type::Float,
                "bool" => ast::Type::Bool,
                "char" => ast::Type::Char,
                "string" => ast::Type::Str,
                "unit" => ast::Type::Unit,

                _ => return Err(Error::new(ErrorType::SyntaxError, format!("Unknown type."), *loc))
            };
            lexed.next();
            return Ok(typ);
        }
        Some((Paren('['), _)) => { //Array type
            lexed.next();
            let element_type = parse_any_type(lexed)?;
            parse_parenthesis(lexed, ']')?;
            Ok(ast::Type::Array(Box::new(element_type)))
        }
        _ => Err(Error::new(ErrorType::SyntaxError, format!("Expected a type."), curr_loc(lexed)?))
    }
}

/// Parses and returns a literal
fn parse_literal(lexed: &mut LexIter) -> KeepRes {
    let loc = curr_loc(lexed)?;

    let lit = match lexed.peek() {
        Some((LexToken::Int(i), _)) => Exp::LiteralExp(Literal::Int(*i), loc),
        Some((LexToken::Float(f), _)) => Exp::LiteralExp(Literal::Float(*f), loc),
        Some((LexToken::Bool(b), _)) => Exp::LiteralExp(Literal::Bool(*b), loc),
        Some((LexToken::Char(c), _)) => Exp::LiteralExp(Literal::Char(*c), loc),
        Some((LexToken::Str(s), _)) => Exp::LiteralExp(Literal::Str(s.clone()), loc),
        Some((_, _))=> return Err(Error::new(ErrorType::SyntaxError, format!("Expected literal"), curr_loc(lexed)?)),
        _ => unreachable!()
    };
    lexed.next();
    Ok(lit)
}

/// Parses and discards a ";"
fn parse_semi_colon(lexed: &mut LexIter) -> DiscardRes {
    match lexed.peek() {
        Some((SemiColon, _)) => {
            lexed.next();
            Ok(())
        },
        _ => Err(Error::new(ErrorType::SyntaxError, format!("Expected ';'."), curr_loc(lexed)?))
    }
}

/// Parses and discards a ":"
fn parse_colon(lexed: &mut LexIter) -> DiscardRes {
    match lexed.peek() {
        Some((Colon, _)) => {
            lexed.next();
            Ok(())
        },
        _ => Err(Error::new(ErrorType::SyntaxError, format!("Expected ':'."), curr_loc(lexed)?))
    }
}

/// Parses and discards a ","
fn parse_comma(lexed: &mut LexIter) -> DiscardRes {
    match lexed.peek() {
        Some((Comma, _)) => {
            lexed.next();
            Ok(())
        },
        _ => Err(Error::new(ErrorType::SyntaxError, format!("Expected ','."), curr_loc(lexed)?))
    }
}

/// Parses and discards parentesis if it matches
fn parse_parenthesis(lexed: &mut LexIter, paren: char) -> DiscardRes {
    match lexed.peek() {
        Some((Paren(par), loc)) => {
            if *par != paren {
                return Err(Error::new(ErrorType::SyntaxError, format!("Expected '{paren}'."), *loc))
            }
            lexed.next();
            Ok(())
        },
        _ => Err(Error::new(ErrorType::SyntaxError, format!("Expected '{paren}'."), curr_loc(lexed)?))
    }
}

/// Determines if the nex token is a "terminator" token
fn terminator(lexed: &mut LexIter) -> bool {
    TERMINATORS.contains(&lexed.peek().unwrap().0)
}

/// Determines if the nex token is a "within" token
fn within_token(lexed: &mut LexIter) -> bool {
    WITHIN_TOKENS.contains(&lexed.peek().unwrap().0)
}

/// Gets the current location in the LexIter
fn curr_loc(lexed: &mut LexIter) -> Result<Location, Error> {
    match lexed.peek() {
        Some((EndOfInput, loc)) => Err(Error::new(ErrorType::SyntaxError, format!("Unexpected end of input."), *loc)),
        Some((_, loc)) => Ok(*loc),
        None => panic!("Could not get location."),
    }
}