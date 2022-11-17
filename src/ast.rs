pub enum Exp {
    BinOpExp(Exp, Operator, Exp),
    UnOpExp(Operator, Exp),
    LiteralExp(Literal),
    BlockExp(Vec<Exp>),

    ///Condition, if true, else
    IfElseExp(Exp, Exp, Exp)
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
    GreaterOrEquals
}

impl Exp {
    pub fn evaluate() {
        
    }
}