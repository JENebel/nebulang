use std::{fmt::Display};

use crate::{lexer::Location, environment::Environment};

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    BinOpExp(Box<Exp>, Operator, Box<Exp>, Location),
    UnOpExp(Operator, Box<Exp>, Location),
    LiteralExp(Literal, Location),
    VarExp(String, Location),
    WhileExp(Box<Exp>, Box<Exp>, Location),

    //let exp, comparison, increment, body
    ForExp(Box<Exp>, Box<Exp>, Box<Exp>, Box<Exp>, Location),

    //Id, exp
    LetExp(String, Box<Exp>, Location),

    ///Condition, if true, else
    IfElseExp(Box<Exp>, Box<Exp>, Option<Box<Exp>>, Location),

    BlockExp(Vec<Exp>, Vec<(String, Box<Function>)>, Location),
    FunCallExp(String, Vec<Exp>, Location),
    FunDeclExp(String, Location)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal {
    //Function(&'a Function),
    Int(i64),
    Float(f64),
    Bool(bool),
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub loc: Location,
    pub ret_type: Type,
    pub param_types: Vec<Type>,
    pub params: Vec<String>,
    pub exp: Box<Exp>
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Type {
    Int,
    Float,
    Bool,
    Unit,

    //Before type check
    Any
}

#[derive(Clone, Debug)]
pub struct Closure<T> {
    pub declared: bool,
    pub fun: Box<Function>,
    pub envir: Environment<T>
}

impl<T> Closure<T> {
    pub fn new(fun: Box<Function>, envir: Environment<T>) -> Self {
        Self { fun, envir, declared: false }
    }

    pub fn decl_scope(&self) -> u32 {
        self.envir.scope_depth
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    LessThan,
    GreaterThan,
    Equals,
    LessOrEquals,
    GreaterOrEquals,
    Not,
    Assign,
    PlusAssign,
    MinusAssign,
    And,
    Or,
    NotEquals
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                Literal::Int(i) => i.to_string(),
                Literal::Float(f) => f.to_string(),
                Literal::Bool(b) => b.to_string(),
                Literal::Unit => "Unit".to_string(),
            }
        )
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                Type::Int => "int",
                Type::Float => "float",
                Type::Bool => "bool",
                Type::Unit => "unit",
                Type::Any => "any",
            }
        )
    }
}

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                Exp::BinOpExp(left, op, right, _) => format!("({left} {op} {right})"),
                Exp::UnOpExp(op, exp, _) => format!("({op}{exp})"),
                Exp::LiteralExp(lit, _) => format!("{lit}"),
                Exp::VarExp(id, _) => format!("{id}"),
                Exp::BlockExp(exps, _, _) => {
                    //Should also output fun defs
                    let mut res = format!("{{");
                    for exp in exps {
                        res = format!("{res}\n {exp}")
                    }
                    res = format!("{res}\n}}");
                    res
                },
                Exp::IfElseExp(cond, pos, neg, _) => match neg {
                    Some(neg) => format!("if({cond}) {pos} else {neg}"),
                    None => format!("if({cond}) {pos}"),
                },
                Exp::LetExp(id, exp, _) =>  format!("let {id} = {exp};"),
                Exp::WhileExp(cond, exp, _) => format!("while({cond})  {exp}"),
                Exp::FunCallExp(_, _, _) => format!("FunCall"),
                Exp::FunDeclExp(_, _) => format!("FunDecl"),
                Exp::ForExp(_, _, _, _, _) => format!("For"),
            }
        )
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Multiply => "*",
                Operator::Divide => "/",
                Operator::Modulo => "%",
                Operator::LessThan => "<",
                Operator::GreaterThan => ">",
                Operator::Equals => "==",
                Operator::LessOrEquals => "<=",
                Operator::GreaterOrEquals => ">=",
                Operator::Not => "!",
                Operator::Assign => "=",
                Operator::PlusAssign => "+=",
                Operator::MinusAssign => "-=",
                Operator::And => "&&",
                Operator::Or => "||",
                Operator::NotEquals => "!=",
            }
        )
    }
}