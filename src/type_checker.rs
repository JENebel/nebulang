use super::*;
use ast::Type::*;
use Operator::*;
use Exp::*;

type TypeResult = Result<ast::Type, (String, Location)>;

impl<'a> Exp {
    pub fn type_check(&'a self, envir: &'a mut Environment<ast::Type>) -> TypeResult {
        match self {
            BinOpExp(left, op, right, loc) => match op {
                Plus => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Int, Int) => Ok(Int),
                    (Int, Float) => Ok(Float),
                    (Float, Int) => Ok(Float),
                    (Float, Float) => Ok(Float),

                    //String
                    (Str, Str) | (Str, Char) | (Char, Str)
                    | (Str, Int) | (Int, Str) | (Str, Float)
                    | (Float, Str) | (Str, Bool) | (Bool, Str)
                    | (Char, Char) => Ok(Str),

                    (left, right) => Err((format!("Invalid operation '{op}' for '{left}' and '{right}'"), *loc)),
                },
                Minus | Multiply | Divide | Modulo => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Int, Int) => Ok(Int),
                    (Int, Float) => Ok(Float),
                    (Float, Int) => Ok(Float),
                    (Float, Float) => Ok(Float),
                    (left, right) => Err((format!("Invalid operation '{op}' for '{left}' and '{right}'"), *loc)),
                },
                LessThan | GreaterThan | LessOrEquals | GreaterOrEquals => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Int, Int) => Ok(Bool),
                    (Int, Float) => Ok(Bool),
                    (Float, Int) => Ok(Bool),
                    (Float, Float) => Ok(Bool),
                    (left, right) => Err((format!("Invalid operation '{op}' for '{left}' and '{right}'"), *loc)),
                },
                Equals | NotEquals => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Int, Int) => Ok(Bool),
                    (Float, Float) => Ok(Bool),
                    (Bool, Bool) => Ok(Bool),
                    (left, right) => Err((format!("Invalid operation '{op}' for '{left}' and '{right}'"), *loc)),
                },
                And | Or => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Bool, Bool) => Ok(Bool),
                    (left, right) => Err((format!("Invalid operation '{op}' for '{left}' and '{right}'"), *loc)),
                },
                Assign => match (left.as_ref(), right.type_check(envir)?) {
                    (VarExp(id, loc), value) => {
                        let typ = match envir.lookup_id(id) {
                            Ok(Value::Var(typ)) => typ,
                            Ok(Value::Fun(_)) => return Err((format!("'{id}' was a function. TODO"), *loc)),
                            Err(_) => return Err((format!("Variable '{id}' does not exist here"), *loc))
                        };
                        if typ != value {
                            Err((format!("Cannot assign '{value}' to '{id}' which is '{typ}'"), *loc))
                        } else {
                            Ok(Unit)
                        }
                    },
                    _ => Err((format!("Left side of assignment must be a variable name"), *loc))
                },
                PlusAssign | MinusAssign => match left.as_ref() {
                    VarExp(id, loc) => {
                        let vexp = Box::new(Exp::VarExp(id.clone(), *loc));
                        let op = match op {
                            PlusAssign => Plus,
                            MinusAssign => Minus,
                            _ => unreachable!()
                        };
                        Exp::BinOpExp(vexp, op, right.clone(), *loc).type_check(envir)?;
                        Ok(Unit)
                    },
                    _ => unreachable!("Not a variable id")
                },
                Not => unreachable!("Not a binary operator"),
            },
            UnOpExp(op, exp, loc) => match op {
                Minus => match exp.type_check(envir)? {
                    Int => Ok(Int),
                    Float => Ok(Float),
                    typ => Err((format!("Unary operator '{op}' is not valid for '{typ}'"), *loc)),
                },
                Not => match exp.type_check(envir)? {
                    Bool => Ok(Bool),
                    typ => Err((format!("Unary operator '{op}' is not valid for '{typ}'"), *loc)),
                },
                _ => unreachable!("Not a unary operator")
            },
            LiteralExp(lit, _) => {
                match lit {
                    Literal::Int(_) => Ok(Int),
                    Literal::Float(_) => Ok(Float),
                    Literal::Bool(_) => Ok(Bool),
                    Literal::Char(_) => Ok(Char),
                    Literal::Str(_) => Ok(Str),
                    Literal::Unit => unreachable!("Unit should not show up as a literal outside of returns"),
                }
            },
            BlockExp(exps, funs, loc) => {
                envir.enter_scope();

                for i in 0..funs.len() {
                    let id = &funs[i].0;

                    if envir.id_exist_in_scope(id) {
                        return Err((format!("Function '{}' already exist in this scope", funs[i].0), *loc))
                    }

                    // It is a waste to initialize envir here, maybe fix some time TODO. Same in eval
                    envir.push_variable(id, Value::Fun(Closure::new(funs[i].1, envir.clone())));
                }

                envir.init_fun_envirs();

                let mut returned: ast::Type = Unit;
                for exp in exps {
                    returned = exp.type_check(envir)?;
                }

                envir.leave_scope();

                Ok(returned)
            },
            VarExp(id, loc) => {
                match envir.lookup_id(&id) {
                    Ok(Value::Var(typ)) => Ok(typ),
                    Ok(Value::Fun(_)) => Err((format!("'{id}' is a function and can not be used like a variable"), *loc)),
                    Err(_) => Err((format!("Variable '{id}' does not exist here"), *loc)),
                }
            },
            LetExp(id, exp, loc) => {
                if envir.id_exist_in_scope(&id) {
                    return Err((format!("Variable '{id}' already exist in this scope"), *loc))
                }
                let value = exp.type_check(envir)?;
                envir.push_variable(id, Value::Var(value)); 
                Ok(Unit)
            },
            IfElseExp(cond, pos, neg, loc) => {
                let cond = cond.type_check(envir)?;
                if cond != Bool {
                    return Err((format!("Condition for if must be boolean, got '{cond}'"), *loc))
                }
                let pos_type = pos.type_check(envir)?;
                if let Some(neg) = neg {
                    let neg_type = neg.type_check(envir)?;
                    if pos_type != neg_type {
                        return Err((format!("If and else branch must have same type, got '{pos_type}' and '{neg_type}'"), *loc))
                    }
                    Ok(pos_type)
                } else {
                    Ok(Unit)
                }
            },
            WhileExp(cond, _, loc) => {
                if cond.type_check(envir)? != Bool {
                    return Err((format!("Condition for while must be boolean, got '{cond}'"), *loc))
                }
                Ok(Unit)
            }
            FunCallExp(id, args, loc) => {
                let closure = match envir.lookup_id(id) {
                    Ok(Value::Fun(clo)) => clo,
                    Ok(Value::Var(typ)) => return Err((format!("Cannot call '{id}' as a function. It has type '{typ}'"), *loc)),
                    Err(_) => return Err((format!("Function '{id}' does not exist here"), *loc))
                };

                let rc = envir.get_fun(closure.fun);
                let func = rc.borrow().clone();

                if func.ret_type == Unknown {
                    return Err((format!("Cannot call '{id}' here. '{id}' needs a type annotation as the call is prior to its definition"), *loc))
                }
                
                if args.len() != func.param_types.len() {
                    return Err((format!("Incorrect argument count. '{id}' takes {} arguments, but {} were given", func.param_types.len(), args.len()), *loc))
                }

                for i in 0..args.len() {
                    let checked_type = args[i].type_check(envir)?;
                    if checked_type != func.param_types[i] {
                        return Err((format!("Incorrect argument type. Expected type '{}' for argument {}, but got {}", func.param_types[i], func.params[i], checked_type), *loc))
                    }
                }

                if !closure.declared {
                    // Enables recursive calls to fun before it is declared
                    let mut renv = envir.get_scope(closure.decl_scope());
                    renv.declare_fun(id);
                    func.type_check(id, *loc, &mut renv)?;
                }

                Ok(func.ret_type)
            },
            FunDeclExp(id, loc) => {
                envir.declare_fun(id);

                // Type check. This will always be a function
                if let Value::Fun(clo) = envir.lookup_id(id).unwrap() {
                    let func = envir.get_fun(clo.fun);

                    let res = func.borrow().type_check(&id, *loc, envir)?;

                    if !func.borrow().annotated {
                        func.borrow_mut().ret_type = res.clone();
                    }
                
                    return Ok(res)
                }

                unreachable!()
            },
            ForExp(let_exp, cond, increment, body, _) => {
                envir.enter_scope();
                let_exp.type_check(envir)?;
                cond.type_check(envir)?;
                increment.type_check(envir)?;
                body.type_check(envir)?;
                envir.leave_scope();
                Ok(Unit)
            },
        }
    }
}

impl<'a> ast::Function {
    pub fn type_check(&self, id: &str, loc: Location, envir: &mut Environment<ast::Type>) -> TypeResult {
        envir.enter_scope();

        for i in 0..self.param_types.len() {
            envir.push_variable(&self.params[i].clone(), Value::Var(self.param_types[i].clone()));
        }

        let res = self.exp.type_check(envir)?;
        
        envir.leave_scope();

        if self.annotated && self.ret_type != res {
            return Err((format!("Return type for {id} does not match annotation, got '{res}' but '{}' was annotated", self.ret_type), loc))
        }

        Ok(res)
    }
}