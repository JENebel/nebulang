use std::fmt::Display;

use crate::lexer::Location;

pub enum Exp {
    BinOpExp(Box<Exp>, Operator, Box<Exp>, Location),
    UnOpExp(Operator, Box<Exp>, Location),
    LiteralExp(Literal, Location),
    BlockExp(Vec<Exp>, Location),

    ///Condition, if true, else
    IfElseExp(Box<Exp>, Box<Exp>, Option<Box<Exp>>, Location)
}

pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool)
}

pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equals,
    LessOrEquals,
    GreaterOrEquals,
    Assign
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                Literal::Int(i) => i.to_string(),
                Literal::Float(f) => f.to_string(),
                Literal::Bool(b) => b.to_string()
            }
        )
    }
}

impl Exp {
    pub fn evaluate(&self) -> Literal {
        Literal::Bool(true)
    }
}