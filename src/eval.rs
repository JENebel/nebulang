use super::*;
use Literal::*;
use Operator::*;
use Exp::*;

pub struct Environment {
    var_stack: Vec<(String, Literal)>,
    scope_sizes: Vec<u16>,
    current_scope_vars: u16
}

impl Environment {
    pub fn new() -> Self {
        Self { 
            var_stack: Vec::new(),
            scope_sizes: Vec::new(),
            current_scope_vars: 0
        }
    }

    pub fn enter_scope(&mut self) {
        self.scope_sizes.push(self.current_scope_vars);
        self.current_scope_vars = 0
    }

    pub fn leave_scope(&mut self) {
        //Pop vars
        for _ in 0..self.current_scope_vars {
            self.var_stack.pop();
        }
        
        //Pop scope size
        self.current_scope_vars = self.scope_sizes.pop().expect("Stack was empty. Should never happen");
    }

    pub fn push_variable(&mut self, id: &String, value: Literal){
        self.var_stack.push((id.clone(), value));
        self.current_scope_vars += 1;
    }

    pub fn lookup(&self, id: &String) -> Literal {
        self.var_stack.iter().rev().find(|var| &var.0 == id).expect("Var id not found. Should never happen").1
    }

    pub fn mutate(&mut self, id: &String, new_value: Literal) {
        for i in (0..self.var_stack.len()).rev() {
            if &self.var_stack[i].0 == id {
                self.var_stack[i].1 = new_value;
            }
        }
    }
}

impl Exp {
    pub fn evaluate(&self, envir: &mut Environment) -> Literal {
        match self {
            BinOpExp(left, op, right, _) => match op {
                Plus => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Int(left + right),
                    (Int(left), Float(right)) => Float(left as f64 + right),
                    (Float(left), Int(right)) => Float(left + right  as f64),
                    (Float(left), Float(right)) => Float(left + right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Minus => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Int(left - right),
                    (Int(left), Float(right)) => Float(left as f64 - right),
                    (Float(left), Int(right)) => Float(left - right  as f64),
                    (Float(left), Float(right)) => Float(left - right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Multiply => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Int(left * right),
                    (Int(left), Float(right)) => Float(left as f64 * right),
                    (Float(left), Int(right)) => Float(left * right  as f64),
                    (Float(left), Float(right)) => Float(left * right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Divide => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Int(left / right),
                    (Int(left), Float(right)) => Float(left as f64 / right),
                    (Float(left), Int(right)) => Float(left / right  as f64),
                    (Float(left), Float(right)) => Float(left / right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Modulo => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Int(left % right),
                    (Int(left), Float(right)) => Float(left as f64 % right),
                    (Float(left), Int(right)) => Float(left % right  as f64),
                    (Float(left), Float(right)) => Float(left % right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                LessThan => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Bool(left < right),
                    (Int(left), Float(right)) => Bool((left as f64) < right),
                    (Float(left), Int(right)) => Bool(left < right  as f64),
                    (Float(left), Float(right)) => Bool(left < right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                GreaterThan => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Bool(left > right),
                    (Int(left), Float(right)) => Bool(left as f64 > right),
                    (Float(left), Int(right)) => Bool(left > right  as f64),
                    (Float(left), Float(right)) => Bool(left > right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Equals => Bool(left.evaluate(envir) == right.evaluate(envir)),
                NotEquals => Bool(left.evaluate(envir) != right.evaluate(envir)),
                LessOrEquals => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Bool(left <= right),
                    (Int(left), Float(right)) => Bool(left as f64 <= right),
                    (Float(left), Int(right)) => Bool(left <= right  as f64),
                    (Float(left), Float(right)) => Bool(left <= right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                GreaterOrEquals => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Int(left), Int(right)) => Bool(left >= right),
                    (Int(left), Float(right)) => Bool(left as f64 >= right),
                    (Float(left), Int(right)) => Bool(left >= right  as f64),
                    (Float(left), Float(right)) => Bool(left >= right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                And => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Bool(left), Bool(right)) => Bool(left && right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Or => match (left.evaluate(envir), right.evaluate(envir)) {
                    (Bool(left), Bool(right)) => Bool(left || right),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Assign => match (left.as_ref(), right.evaluate(envir)) {
                    (VarExp(id, _), value) => {
                        envir.mutate(id, value);
                        Unit
                    },
                    _ => unreachable!("Not a variable id")
                },
                PlusAssign => match (left.as_ref(), right.evaluate(envir)) {
                    (VarExp(id, _), value) => {
                        let new_value = match (envir.lookup(id), value) {
                            (Int(i), Int(y)) => Int(i+y),
                            (Float(f), Float(g)) => Float(f+g),
                            _ => unreachable!("Invalid types")
                        };
                        envir.mutate(id, new_value);
                        Unit
                    },
                    _ => unreachable!("Not a variable id")
                },
                MinusAssign => match (left.as_ref(), right.evaluate(envir)) {
                    (VarExp(id, _), value) => {
                        let new_value = match (envir.lookup(id), value) {
                            (Int(i), Int(y)) => Int(i-y),
                            (Float(f), Float(g)) => Float(f-g),
                            _ => unreachable!("Invalid types")
                        };
                        envir.mutate(id, new_value);
                        Unit
                    },
                    _ => unreachable!("Not a variable id")
                },
                _ => unreachable!("Not a binary operator: '{op}'")
            },
            UnOpExp(op, exp, _) => match op {
                Minus => match exp.evaluate(envir) {
                    Int(i) => Int(-i),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Not => match exp.evaluate(envir) {
                    Bool(i) => Bool(!i),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                _ => unreachable!("Not a unary operator")
            },
            LiteralExp(lit, _) => *lit,
            BlockExp(exps, _) => {
                envir.enter_scope();

                let mut iter = exps.iter().peekable();
                if iter.peek().is_none() { return Unit }

                let mut returned = Unit;
                while let Some(exp) = iter.next() {
                    returned = exp.evaluate(envir);
                    if iter.peek().is_none() {
                        envir.leave_scope();
                        return returned;
                    }
                }

                envir.leave_scope();

                returned
            },
            VarExp(id, _) => envir.lookup(&id),
            LetExp(id, exp, _) => {
                let value = exp.evaluate(envir);
                envir.push_variable(&id, value); 
                Unit
            },
            IfElseExp(cond, pos, neg, _) => {
                let cond = cond.evaluate(envir);
                let res = match cond {
                    Bool(true) => pos.evaluate(envir), //This should return unit if neg is none. This should not be a problem after type check
                    Bool(false) => if let Some(exp) = neg {
                        exp.evaluate(envir)
                    } else {
                        Unit
                    },
                    _ => unreachable!("Condition must be a bool. Shouldn't happen at runtime")
                };

                res
            },
            WhileExp(cond, exp, _) => {
                loop {
                    let res = cond.evaluate(envir);
                    match res {
                        Bool(true) => exp.evaluate(envir),
                        Bool(false) => break,
                        _ => panic!("Condition must be a bool")
                    };
                }

                Unit
            },
        }
    }
}