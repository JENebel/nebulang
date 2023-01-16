use crate::ast::*;
use crate::environment::*;

use Literal::*;
use Operator::*;
use Exp::*;

impl<'a> Exp {
    pub fn evaluate(&'a self, envir: &'a mut Environment<Literal>) -> Result<Literal, Error> {
        match self {
            BinOpExp(left, op, right, loc) => match op {
                Plus => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Int(left), Int(right)) => Ok(Int(left + right)),
                    (Int(left), Float(right)) => Ok(Float(left as f64 + right)),
                    (Float(left), Int(right)) => Ok(Float(left + right  as f64)),
                    (Float(left), Float(right)) => Ok(Float(left + right)),

                    (Str(left), Str(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Str(left), Char(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Char(left), Str(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Str(left), Int(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Int(left), Str(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Str(left), Float(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Float(left), Str(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Str(left), Bool(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Bool(left), Str(right)) => Ok(Str(format!("{}{}", left, right))),
                    (Char(left), Char(right)) => Ok(Str(format!("{}{}", left, right))),

                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Minus => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Int(left), Int(right)) => Ok(Int(left - right)),
                    (Int(left), Float(right)) => Ok(Float(left as f64 - right)),
                    (Float(left), Int(right)) => Ok(Float(left - right  as f64)),
                    (Float(left), Float(right)) => Ok(Float(left - right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Multiply => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Int(left), Int(right)) => Ok(Int(left * right)),
                    (Int(left), Float(right)) => Ok(Float(left as f64 * right)),
                    (Float(left), Int(right)) => Ok(Float(left * right  as f64)),
                    (Float(left), Float(right)) => Ok(Float(left * right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Divide => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (left, Int(0)) => Err(Error::new(ErrorType::RuntimeError, format!("Tried to divide by zero: {left}/0."), *loc)),
                    (Int(left), Int(right)) => Ok(Int(left / right)),
                    (Int(left), Float(right)) => Ok(Float(left as f64 / right)),
                    (Float(left), Int(right)) => Ok(Float(left / right  as f64)),
                    (Float(left), Float(right)) => Ok(Float(left / right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Modulo => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Int(left), Int(right)) => Ok(Int(left % right)),
                    (Int(left), Float(right)) => Ok(Float(left as f64 % right)),
                    (Float(left), Int(right)) => Ok(Float(left % right  as f64)),
                    (Float(left), Float(right)) => Ok(Float(left % right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                LessThan => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Int(left), Int(right)) => Ok(Bool(left < right)),
                    (Int(left), Float(right)) => Ok(Bool((left as f64) < right)),
                    (Float(left), Int(right)) => Ok(Bool(left < right  as f64)),
                    (Float(left), Float(right)) => Ok(Bool(left < right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                GreaterThan => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Int(left), Int(right)) => Ok(Bool(left > right)),
                    (Int(left), Float(right)) => Ok(Bool(left as f64 > right)),
                    (Float(left), Int(right)) => Ok(Bool(left > right  as f64)),
                    (Float(left), Float(right)) => Ok(Bool(left > right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Equals => Ok(Bool(left.evaluate(envir)? == right.evaluate(envir)?)),
                NotEquals => Ok(Bool(left.evaluate(envir)? != right.evaluate(envir)?)),
                LessOrEquals => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Int(left), Int(right)) => Ok(Bool(left <= right)),
                    (Int(left), Float(right)) => Ok(Bool(left as f64 <= right)),
                    (Float(left), Int(right)) =>Ok( Bool(left <= right  as f64)),
                    (Float(left), Float(right)) => Ok(Bool(left <= right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                GreaterOrEquals => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Int(left), Int(right)) => Ok(Bool(left >= right)),
                    (Int(left), Float(right)) => Ok(Bool(left as f64 >= right)),
                    (Float(left), Int(right)) => Ok(Bool(left >= right  as f64)),
                    (Float(left), Float(right)) => Ok(Bool(left >= right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                And => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Bool(left), Bool(right)) => Ok(Bool(left && right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Or => match (left.evaluate(envir)?, right.evaluate(envir)?) {
                    (Bool(left), Bool(right)) => Ok(Bool(left || right)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Assign => match (left.as_ref(), right.evaluate(envir)?) {
                    (VarExp(id, _), value) => {
                        envir.mutate(id, Value::Var(value));
                        Ok(Unit)
                    },
                    _ => unreachable!("Not a variable id")
                },
                PlusAssign | MinusAssign => match left.as_ref() {
                    VarExp(id, loc) => {
                        let other = Box::new(Exp::LiteralExp(right.evaluate(envir)?, *loc));
                        let vexp = Box::new(Exp::VarExp(id.clone(), *loc));
                        let op = match op {
                            PlusAssign => Plus,
                            MinusAssign => Minus,
                            _ => unreachable!()
                        };
                        let new_value = Exp::BinOpExp(vexp, op, other, *loc).evaluate(envir)?;
                        envir.mutate(id, Value::Var(new_value));
                        Ok(Unit)
                    },
                    _ => unreachable!("Not a variable id")
                },
                _ => unreachable!("Not a binary operator: '{op}'")
            },
            UnOpExp(op, exp, _) => match op {
                Minus => match exp.evaluate(envir)? {
                    Int(i) => Ok(Int(-i)),
                    Float(i) => Ok(Float(-i as f64)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                Not => match exp.evaluate(envir)? {
                    Bool(i) => Ok(Bool(!i)),
                    _ => unreachable!("Runtime type-error should not happen"),
                },
                _ => unreachable!("Not a unary operator")
            },
            LiteralExp(lit, _) => Ok(lit.clone()),
            BlockExp(exps, funs, _) => {
                envir.enter_scope();

                for fun in funs {
                    // It is a waste to initialize envir here, maybe fix some time TODO. Same in type checker
                    envir.push_variable(&fun.0, Value::Fun(Closure::new(fun.1, envir.clone())))
                }

                envir.init_fun_envirs();

                let mut returned = Unit;
                for exp in exps {
                    returned = exp.evaluate(envir)?;
                }
                envir.leave_scope();

                Ok(returned)
            },
            VarExp(id, _) => if let Value::Var(var) = envir.lookup_id(id).unwrap() {
                Ok(var)
            } else {
                panic!("Expected '{id}' was a variable, but found function")
            },
            LetExp(id, exp, _) => {
                let value = exp.evaluate(envir)?;
                envir.push_variable(id, Value::Var(value)); 
                Ok(Unit)
            },
            IfElseExp(cond, pos, neg, _) => {
                let cond = cond.evaluate(envir)?;
                let res = match cond {
                    Bool(true) => pos.evaluate(envir)?, //This should return unit if neg is none. This should not be a problem after type check
                    Bool(false) => if let Some(exp) = neg {
                        exp.evaluate(envir)?
                    } else {
                        Unit
                    },
                    _ => unreachable!("Condition must be a bool. Shouldn't happen at runtime")
                };

                Ok(res)
            },
            WhileExp(cond, exp, _) => {
                loop {
                    let res = cond.evaluate(envir)?;
                    match res {
                        Bool(true) => exp.evaluate(envir)?,
                        Bool(false) => break,
                        _ => panic!("Condition must be a bool")
                    };
                }

                Ok(Unit)
            }
            FunCallExp(id, args, _) => {
                // Retrieve closure and function
                let mut closure = match envir.lookup_id(id) {
                    Ok(Value::Fun(clo)) => clo,
                    Ok(Value::Var(_)) => panic!(),      //return Err((format!("Cannot call '{id}' as a function. It has type '{typ}'"), *loc)),
                    Err(_) => panic!()                  //return Err((format!("Function '{id}' does not exist here"), *loc))
                };
                
                let func = envir.get_fun(closure.fun);

                // Evaluate parameters
                let mut lits = Vec::new();
                for i in 0..args.len() {
                    lits.push(args[i].evaluate(envir)?);
                }

                //If it is not declared, it takes the most recent scope from decl scope
                let res: Literal = if !closure.declared {
                    let mut renv = envir.get_scope(closure.decl_scope());
                    for i in 0..args.len() {
                        renv.push_variable(&func.borrow().params[i], Value::Var(lits[i].clone()));
                    }
                    func.borrow().exp.evaluate(&mut renv)?
                } else {
                    closure.envir.enter_scope();
                    for i in 0..args.len() {
                        closure.envir.push_variable(&func.borrow().params[i].clone(), Value::Var(lits[i].clone()));
                    }
                    let res = func.borrow().exp.evaluate(&mut closure.envir)?;
                    closure.envir.leave_scope();
                    res
                };

                if func.borrow().ret_type == Type::Unit {
                    Ok(Unit)
                } else {
                    Ok(res)
                }
            },
            FunDeclExp(id, _) => {
                envir.declare_fun(&id);
                Ok(Unit)
            },
            ForExp(let_exp, cond, increment, body, _) => {
                envir.enter_scope();
                let_exp.evaluate(envir)?;

                loop {
                    if let Literal::Bool(false) = cond.evaluate(envir)? {
                        break;
                    }
                    body.evaluate(envir)?;
                    increment.evaluate(envir)?;
                }

                envir.leave_scope();

                Ok(Unit)
            },
        }
    }
}