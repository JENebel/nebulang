use std::fmt::Display;

use crate::lexer::*;
use crate::environment::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    /// (left, operator, right, loc)
    BinOpExp(Box<Exp>, Operator, Box<Exp>, Location),

    /// (operator, value-loc)
    UnOpExp(Operator, Box<Exp>, Location),

    /// (literal, loc)
    LiteralExp(Literal, Location),

    /// (var-id, loc)
    VarExp(String, Location),

    /// (condition, body, loc)
    WhileExp(Box<Exp>, Box<Exp>, Location),

    /// (var-decl(let), condition, increment, body, loc)
    ForExp(Box<Exp>, Box<Exp>, Box<Exp>, Box<Exp>, Location),

    /// (id, value, loc)
    LetExp(String, Box<Exp>, Location),

    /// (condition, if-true, if-false, loc)
    IfElseExp(Box<Exp>, Box<Exp>, Option<Box<Exp>>, Location),

    /// (statements, functions, loc)
    BlockExp(Vec<Exp>, Vec<(String, usize)>, Location),

    /// (fun-id, arguments, loc)
    FunCallExp(String, Vec<Exp>, Location),

    /// (fun-id, loc)
    FunDeclExp(String, Location)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    // Function(&'a Function),
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub annotated: bool,
    pub loc: Location,
    pub ret_type: Type,
    pub param_types: Vec<Type>,
    pub params: Vec<String>,
    pub exp: Box<Exp>
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Int,
    Float,
    Bool,
    Unit,
    Char,
    Str,

    /// A type value
    //Type(Box<Type>),
    /// (params, return-type)
    //Function(Vec<Type>, Box<Type>),

    /// Before type check. Should not show up in evaluation!
    Unknown
}

#[derive(Debug, Clone)]
pub struct Closure<T: Clone + Display> {
    /// Whether or not the function has been declared yet
    pub declared: bool,

    /// Function store index
    pub fun: usize,
    pub envir: Environment<T>
}

impl<T: Clone + Display> Closure<T> {
    pub fn set_envir(&mut self, new_envir: Environment<T>) {
        self.envir = new_envir;
    }
}

impl<T: Clone + Display> Display for Closure<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(Decl: {}, index: {})", self.declared, self.fun)
    }
}

impl<T: Clone + Display> Closure<T> {
    pub fn new(fun: usize, envir: Environment<T>) -> Self {
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
                Literal::Char(c) => format!("'{}'", c),
                Literal::Str(s) => format!("\"{}\"", s),
                Literal::Unit => format!("Unit"),
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
                Type::Char => "char",
                Type::Str => "string",
                Type::Unit => "unit",
                Type::Unknown => "any",
                //_ => todo!()
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