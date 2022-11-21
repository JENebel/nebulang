use std::fmt::{Display, format};

use crate::lexer::Location;

#[derive(Debug)]
#[derive(Clone)]
pub enum Exp {
    BinOpExp(Box<Exp>, Operator, Box<Exp>, Location),
    UnOpExp(Operator, Box<Exp>, Location),
    LiteralExp(Literal, Location),
    BlockExp(Vec<Exp>, Location),

    ///Condition, if true, else
    IfElseExp(Box<Exp>, Box<Exp>, Option<Box<Exp>>, Location)
}

#[derive(Copy, Clone)]
#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool)
}

#[derive(Debug)]
#[derive(Copy, Clone)]
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
    Not
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

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                Exp::BinOpExp(left, op, right, _) => format!("({left} {op} {right})"),
                Exp::UnOpExp(op, exp, _) => format!("({op}{exp})"),
                Exp::LiteralExp(lit, _) => format!("{lit}"),
                Exp::BlockExp(exps, _) => {
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
                Operator::Not => "!"
            }
        )
    }
}

impl Exp {
    pub fn evaluate(&self) -> Literal {
        Literal::Bool(true)
    }
}