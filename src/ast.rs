use std::cell::RefCell;
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

    /// (let_exp, cond_exp, inc_exp, body, loc)
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
    FunDeclExp(String, Location),

    /// Initializes a new Array object and returns this
    /// 
    /// (length, template, loc)
    InitArrayExp(Box<Exp>, Box<Exp>, Location),
}

pub struct Error {
    pub error_type: ErrorType,
    pub error_msg: String,
    pub location: Location,
}

impl Error {
    pub fn new(error_type: ErrorType, error_msg: String, location: Location) -> Self {
        Self { error_type, error_msg, location }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {} At {}", self.error_type, self.error_msg, self.location)
    }
}

#[derive(PartialEq)]
pub enum ErrorType {
    SyntaxError,
    TypeError,

    RuntimeError,
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                ErrorType::SyntaxError => "Syntax Error",
                ErrorType::TypeError => "Type Error",
                ErrorType::RuntimeError => "Runtime Error",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    // Function(&'a Function),
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),

    /// (Values, Template)
    Array(Vec<Option<RefCell<Literal>>>, Box<Literal>),
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
    Array(Box<Type>),

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
    NotEquals,
}

impl Literal {
    pub fn get_type(&self) -> Type {
        match self {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
            Literal::Char(_) => Type::Char,
            Literal::Str(_) => Type::Str,
            Literal::Array(_, template) => Type::Array(Box::new(template.get_type())),
            Literal::Unit => unreachable!("Unit should not show up as a literal outside of returns."),
        }
    }
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
                Literal::Array(arr, template) => {
                    let mut res = String::new();
                    for lit in arr {
                        let lit = match lit {
                            Some(lit) => format!("{}", lit.borrow()),
                            None => format!("{template}"),
                        };

                        if res.is_empty() {
                            res = format!("{lit}")
                        } else {
                            res = format!("{res}, {lit}")
                        }
                    }
                    res = format!("[{res}]");
                    res
                },
                Literal::Unit => format!("unit"),
            }
        )
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                Type::Int => format!("int"),
                Type::Float => format!("float"),
                Type::Bool => format!("bool"),
                Type::Char => format!("char"),
                Type::Str => format!("string"),
                Type::Array(typ) => format!("[{typ}]"),
                Type::Unit => format!("unit"),
                Type::Unknown => format!("any"),
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
                    //Should also output fun defs TODO
                    let mut res = String::new();
                    for exp in exps {
                        res = format!("{res}\n {exp}")
                    }
                    res = format!("{{{res}\n}}");
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
                Exp::InitArrayExp(length, template, _,) => format!("[{length} of {template}]"),
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