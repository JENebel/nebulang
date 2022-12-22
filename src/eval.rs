use super::*;
use Literal::*;
use Operator::*;
use Exp::*;

impl<'a> Exp {
    pub fn evaluate(&'a self, envir: &'a mut Environment<Literal>) -> Literal {
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
                        let new_value = match (envir.lookup_var(id).unwrap(), value) {
                            (Int(i), Int(y)) => Int(i+y),
                            (Float(f), Float(g)) => Float(f+g),
                            (Float(f), Int(g)) => Float(f+g as f64),
                            _ => unreachable!("Invalid types"),
                        };
                        envir.mutate(id, new_value);
                        Unit
                    },
                    _ => unreachable!("Not a variable id")
                },
                MinusAssign => match (left.as_ref(), right.evaluate(envir)) {
                    (VarExp(id, _), value) => {
                        let new_value = match (envir.lookup_var(id).unwrap(), value) {
                            (Int(i), Int(y)) => Int(i-y),
                            (Float(f), Float(g)) => Float(f-g),
                            (Float(f), Int(g)) => Float(f-g as f64),
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
                    Float(i) => Float(-i as f64),
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
                envir.enter_scope();
                for fun in funs {
                    envir.push_function(fun.0.clone(), fun.1.clone())
                }

                envir.update_fun_envirs();

                let mut returned = Unit;
                for exp in exps {
                    returned = exp.evaluate(envir);
                }
                envir.leave_scope();

                returned
            },
            VarExp(id, _) => envir.lookup_var(&id).unwrap(),
            LetExp(id, exp, _) => {
                let value = exp.evaluate(envir);
                envir.push_variable(id.clone(), value); 
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
                let mut lits = Vec::new();
                for i in 0..args.len() {
                    lits.push(args[i].evaluate(envir));
                }

                let mut closure = envir.lookup_fun(id).unwrap();
                //If it is not declared, it takes the most recent scope from decl scope
                let res: Literal = if closure.declared {
                    closure.envir.enter_scope();
                    for i in 0..args.len() {
                        closure.envir.push_variable(closure.fun.params[i].clone(), lits[i]);
                    }
                    let res = closure.fun.exp.evaluate(&mut closure.envir);
                    closure.envir.leave_scope();
                    res
                } else {
                    let mut envir = envir.get_scope(closure.decl_scope());
                    for i in 0..args.len() {
                        envir.push_variable(closure.fun.params[i].clone(), lits[i]);
                    }
                    closure.fun.exp.evaluate(&mut envir)
                };

                if closure.fun.ret_type == ast::Type::Unit {
                    Unit
                } else {
                    res
                }
            },
            FunDeclExp(id, _) => {
                envir.declare_fun(&id);
                Unit
            },
            ForExp(let_exp, cond, increment, body, _) => {
                envir.enter_scope();
                let_exp.evaluate(envir);

                loop {
                    if let Literal::Bool(false) = cond.evaluate(envir) {
                        break;
                    }
                    body.evaluate(envir);
                    increment.evaluate(envir);
                }

                envir.leave_scope();

                Unit
            },
        }
    }
}