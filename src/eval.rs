use std::rc::Rc;

use super::*;
use Literal::*;
use Operator::*;
use Exp::*;

pub struct Environment {
    var_stack: Vec<(String, Literal)>,
    var_scope_sizes: Vec<u16>,
    var_current_scope: u16,

    fun_stack: Vec<(String, Rc<Function>)>,
    fun_scope_sizes: Vec<u16>,
    fun_current_scope: u16
}

impl Environment {
    pub fn new() -> Self {
        Self { 
            var_stack: Vec::new(),
            var_scope_sizes: Vec::new(),
            var_current_scope: 0,

            fun_stack: Vec::new(),
            fun_scope_sizes: Vec::new(),
            fun_current_scope: 0
        }
    }

    pub fn enter_scope (&mut self, funs: &Vec<(String, Rc<Function>)>) {
        self.var_scope_sizes.push(self.var_current_scope);

        self.var_current_scope = 0;

        self.fun_scope_sizes.push(self.fun_current_scope);
        for i in 0..funs.len() {
            self.fun_current_scope += 1;
            self.fun_stack.push((funs[i].0.clone(), funs[i].1.clone()))
        }
    }

    pub fn leave_scope(&mut self) {
        //Pop vars
        for _ in 0..self.var_current_scope {
            self.var_stack.pop();
        }

        //Pop funs
        for _ in 0..self.fun_current_scope {
            self.fun_stack.pop();
        }
        
        //Pop scope size
        self.var_current_scope = self.var_scope_sizes.pop().expect("Var stack was empty. Should never happen");
        self.fun_current_scope = self.fun_scope_sizes.pop().expect("Fun stack was empty. Should never happen");
    }

    pub fn push_variable(&mut self, id: &String, value: Literal) {
        self.var_current_scope += 1;
        self.var_stack.push((id.clone(), value));
    }

    pub fn lookup_var(&self, id: &String) -> &Literal {
        &self.var_stack.iter().rev().find(|var| &var.0 == id).expect("Var id not found. Should never happen").1
    }

    pub fn lookup_fun(&self, id: &String) -> &Function {
        &self.fun_stack.iter().rev().find(|var| var.0 == *id).expect("Fun id not found. Should never happen").1
    }

    pub fn mutate(&mut self, id: &String, new_value: Literal) {
        for i in (0..self.var_stack.len()).rev() {
            if &self.var_stack[i].0 == id {
                self.var_stack[i].1 = new_value;
            }
        }
    }
}

impl<'a> Exp {
    pub fn evaluate(&'a self, envir: &'a mut Environment) -> Literal {
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
                        let new_value = match (envir.lookup_var(id), value) {
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
                        let new_value = match (envir.lookup_var(id), value) {
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
            BlockExp(exps, funs, _) => {
                envir.enter_scope(&funs);

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
            VarExp(id, _) => *envir.lookup_var(&id),
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
            }
            FunCallExp(id, args, _) => {
                envir.enter_scope(&Vec::new());

                let func = envir.lookup_fun(id).clone();
                
                for i in 0..args.len() {
                    let lit = args[i].evaluate(envir);
                    envir.push_variable(&func.params[i], lit);
                }

                let res = func.exp.evaluate(envir);

                envir.leave_scope();

                res
            },
        }
    }
}