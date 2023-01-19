use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::lexer::*;
use crate::environment::*;

/// Provides ability to 'deep copy' a value
pub trait DeepCopy {
    fn deep_copy(&self) -> Self;
}

////////////////////
/// # Literal
////////////////////
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    // Function(&'a Function),
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
    Ref(Rc<RefCell<Literal>>),

    /// (Values, Template)
    ArrayLit(Array),
    Unit,

    ReturnedLit(Box<Literal>),
    Break,
    Continue
}

impl Literal {
    /// Returns the type of the literal
    pub fn get_type(&self) -> Type {
        match self {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
            Literal::Char(_) => Type::Char,
            Literal::Str(_) => Type::Str,
            Literal::ArrayLit(arr) => arr.get_type(),
            Literal::Ref(inner) => Type::Ref(Box::new(inner.borrow().get_type())),
            Literal::Unit => Type::Unit,
            Literal::ReturnedLit(_) | Literal::Break | Literal::Continue => Type::Any,
        }
    }

    /// Returns true if it is a control flow literal. Fx break or return
    pub fn is_flow_control_literal(&self) -> bool {
        match self {
            Literal::ReturnedLit(_) => true,
            Literal::Break => true,
            Literal::Continue => true,
            _ => false,
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
                Literal::ArrayLit(arr) => format!("{arr}"),
                Literal::Ref(inner) => format!("&{}", inner.borrow()),
                Literal::Unit => format!("unit"),
                Literal::ReturnedLit(lit) => format!("return({lit})"),
                Literal::Break => panic!("Tried to display a 'break' control flow literal"),
                Literal::Continue => panic!("Tried to display a 'continue' control flow literal"),
            }
        )
    }
}

impl DeepCopy for Literal {
    fn deep_copy(&self) -> Self {
        match self {
            Literal::ArrayLit(arr) => Literal::ArrayLit(arr.deep_copy()),
            _ => self.clone(),
        }
    }
}

////////////////////
/// # Type
////////////////////
#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Float,
    Bool,
    Unit,
    Char,
    Str,
    Array(Box<Type>),
    Ref(Box<Type>),

    /// A type value
    //Type(Box<Type>),
    /// (params, return-type)
    //Function(Vec<Type>, Box<Type>),

    /// Is equal to all types. Returned by return, break, continue.
    /// 
    /// Also the return type of an unannotated function before type check
    Any
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
                Type::Ref(inner) => format!("&{inner}"),
                Type::Unit => format!("unit"),
                Type::Any => format!("any"),
            }
        )
    }
}

impl DeepCopy for Type {
    fn deep_copy(&self) -> Self {
        self.clone()
    }
}

////////////////////
/// # Expression
////////////////////
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

    /// (body, finally(increment), loc)
    LoopExp(Box<Exp>, Option<Box<Exp>>, Location),

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

    /// Initializes a new Array object with template and returns this
    /// 
    /// (length, template, loc)
    InitTemplateArrayExp(Box<Exp>, Box<Exp>, Location),

    /// Initializes a new Array object with initial given values
    /// 
    /// (initial_values, loc)
    InitArrayWithValuesExp(Vec<Exp>, Location),

    /// (array, index, loc)
    AccessArrayExp(Box<Exp>, Box<Exp>, Location),

    /// (value, loc)
    ReturnExp(Box<Exp>, Location),

    /// (loc)
    BreakExp(Location),

    /// (loc)
    ContinueExp(Location),
}

impl Exp {
    pub fn loc(&self) -> Location {
        match self {
            Exp::BinOpExp(_, _, _, loc) |
            Exp::UnOpExp(_, _, loc) |
            Exp::LiteralExp(_, loc) |
            Exp::VarExp(_, loc) |
            Exp::LoopExp(_, _, loc) |
            Exp::LetExp(_, _, loc) |
            Exp::IfElseExp(_, _, _, loc) |
            Exp::BlockExp(_, _, loc) |
            Exp::FunCallExp(_, _, loc) |
            Exp::FunDeclExp(_, loc) |
            Exp::InitTemplateArrayExp(_, _, loc) |
            Exp::InitArrayWithValuesExp(_, loc) |
            Exp::AccessArrayExp(_, _, loc) |
            Exp::ReturnExp(_, loc) |
            Exp::BreakExp(loc) |
            Exp::ContinueExp(loc) 
                => *loc
        }
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
                    Some(neg) => format!("if({cond}) {pos} else {neg};"),
                    None => format!("if({cond}) {pos};"),
                },
                Exp::LetExp(id, exp, _) =>  format!("let {id} = {exp};"),
                Exp::LoopExp(exp, _, _) => format!("loop {{{exp}}}"), // Not showing the inc exp TODO
                Exp::FunCallExp(_, _, _) => format!("FunCall"), //TODO
                Exp::FunDeclExp(_, _) => format!("FunDecl"),    //TODO
                Exp::AccessArrayExp(arr_exp, index_exp, _,) => format!("{arr_exp}[{index_exp}]"),
                Exp::InitTemplateArrayExp(length, template, _,) => format!("[{length} of {template}]"),
                Exp::InitArrayWithValuesExp(values, _,) => {
                    let mut res = String::new();
                    for lit in values {
                        if res.is_empty() {
                            res = format!("{lit}")
                        } else {
                            res = format!("{res}, {lit}")
                        }
                    }
                    res = format!("[{res}]");
                    res
                },
                Exp::ReturnExp(exp, _) => format!("return {exp};"),
                Exp::BreakExp(_) => format!("break;"),
                Exp::ContinueExp(_) => format!("continue;"),
            }
        )
    }
}

////////////////////
/// # Operator
////////////////////
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
    DivideAssign,
    MultiplyAssign,    
    And,
    Or,
    NotEquals,
}

impl Operator {
    /// Strips the 'assign' part of the operator:\
    /// PlusAssign => Plus
    pub fn strip_assign(&self) -> Operator {
        match self {
            Self::PlusAssign => Self::Plus,
            Self::MinusAssign => Self::Minus,
            Self::DivideAssign => Self::Divide,
            Self::MultiplyAssign => Self::Multiply,
            op => *op
        }
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
                Operator::DivideAssign => "/=",
                Operator::MultiplyAssign => "*=",
                Operator::And => "&&",
                Operator::Or => "||",
                Operator::NotEquals => "!=",
            }
        )
    }
}

////////////////////
/// # Error struct
////////////////////
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
        write!(f, "{} at {}: {}", self.error_type, self.location, self.error_msg)
    }
}

/// Type of error
#[derive(PartialEq)]
pub enum ErrorType {
    SyntaxError,
    TypeError,

    RuntimeError,
}

impl Type {
    /// Determines if the type represents a value.
    /// Unit, return, break and continue are not
    pub fn is_value_type(&self) -> bool {
        match self {
            Type::Unit => false,
            _ => true
        }
    }

    pub fn is_any(&self) -> bool {
        match self {
            Type::Any => true,
            _ => false
        }
    }
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

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Any, _) => true,
            (_, Self::Any) => true,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::Ref(l0), Self::Ref(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

////////////////////
/// # Array struct
////////////////////
#[derive(Clone, Debug, PartialEq)]
pub struct Array {
    vec: Rc<RefCell<Vec<Option<Literal>>>>, 
    template: Box<Literal>
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            {
                let mut res = String::new();
                for lit in self.vec.borrow().clone() {
                    let lit = match lit {
                        Some(lit) => format!("{}", lit),
                        None => format!("{}", self.template),
                    };

                    if res.is_empty() {
                        res = format!("{lit}")
                    } else {
                        res = format!("{res}, {lit}")
                    }
                }
                res = format!("[{res}]");
                res
            }
        )
    }
}

impl Array {
    pub fn new(length: usize, template: Literal) -> Self {
        Self { vec: Rc::new(RefCell::new(vec![None; length])), template: Box::new(template) }
    }

    pub fn new_from_values(values: Vec<Literal>, template: Literal) -> Self {
        Self { vec: Rc::new(RefCell::new(values.iter().map(|lit| Some(lit.clone())).collect())), template: Box::new(template) }
    }

    pub fn get_type(&self) -> Type {
        Type::Array(Box::new(self.template.get_type()))
    }

    pub fn get_index(&self, index: usize) -> Literal {
        match &self.vec.borrow()[index] {
            Some(lit) => lit.clone(),
            None => {
                *self.template.clone()
            },
        }
    }

    pub fn set_index(&self, index: usize, new_value: Literal) {
        self.vec.borrow_mut()[index] = Some(new_value.deep_copy())
    }

    pub fn length(&self) -> usize {
        self.vec.borrow().len()
    }

    pub fn deep_copy(&self) -> Self {
        let copy = Array::new(self.length(), *self.template.clone());

        for i in 0..self.length() {
            if let Some(lit) = &self.vec.borrow()[i] {
                 copy.set_index(i, lit.deep_copy())
            }
        }

        copy
    }
}

////////////////////
/// # Function struct
////////////////////
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub annotated: bool,
    pub loc: Location,
    pub ret_type: Type,
    pub param_types: Vec<Type>,
    pub params: Vec<String>,
    pub exp: Box<Exp>
}

////////////////////
/// # Closure struct
////////////////////
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