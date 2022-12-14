use super::*;
use Type::*;
use Operator::*;
use Exp::*;

#[derive(Debug)]
pub struct TypeEnvironment {
    var_stack: Vec<(String, Type)>,
    var_scope_sizes: Vec<u16>,
    var_current_scope: u16,

    fun_stack: Vec<(String, Box<Function>)>,
    fun_scope_sizes: Vec<u16>,
    fun_current_scope: u16
}

impl TypeEnvironment {
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

    pub fn enter_scope (&mut self) {
        self.var_scope_sizes.push(self.var_current_scope);
        self.fun_scope_sizes.push(self.fun_current_scope);

        self.var_current_scope = 0;
        self.fun_current_scope = 0;
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
        self.var_current_scope = self.var_scope_sizes.pop().expect("TYPES Var stack was empty. Should never happen");
        self.fun_current_scope = self.fun_scope_sizes.pop().expect("TYPES Fun stack was empty. Should never happen");
    }

    pub fn push_variable(&mut self, id: String, value: Type) {
        self.var_current_scope += 1;
        self.var_stack.push((id, value));
    }

    pub fn push_function(&mut self, id: String, value: Box<Function>) {
        self.fun_current_scope += 1;
        self.fun_stack.push((id, value));
    }

    pub fn lookup_var(&self, id: &String) -> Result<Type, String> {
        match self.var_stack.iter().rev().find(|var| &var.0 == id) {
            Some(typ) => Ok(typ.1),
            None => Err(format!("Var {id} not found")),
        }
    }

    pub fn lookup_fun(&self, id: &String) -> Result<Box<Function>, String> {
        match self.fun_stack.iter().rev().find(|fun| &fun.0 == id) {
            Some(fun) => Ok(fun.1.clone()),
            None => Err(format!("Fun {id} not found")),
        }
    }
}

type TypeResult = Result<Type, (String, Location)>;

impl<'a> Exp {
    pub fn type_check(&'a mut self, envir: &'a mut TypeEnvironment) -> TypeResult {
        match self {
            BinOpExp(left, op, right, loc) => match op {
                Plus | Minus | Multiply | Divide | Modulo => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Int, Int) => Ok(Int),
                    (Int, Float) => Ok(Float),
                    (Float, Int) => Ok(Float),
                    (Float, Float) => Ok(Float),
                    (left, right) => Err((format!("Invalid operation {op} for {left} and {right}"), *loc)),
                },
                LessThan | GreaterThan | LessOrEquals | GreaterOrEquals => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Int, Int) => Ok(Bool),
                    (Int, Float) => Ok(Bool),
                    (Float, Int) => Ok(Bool),
                    (Float, Float) => Ok(Bool),
                    (left, right) => Err((format!("Invalid operation {op} for {left} and {right}"), *loc)),
                },
                Equals | NotEquals => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Int, Int) => Ok(Bool),
                    (Float, Float) => Ok(Bool),
                    (Bool, Bool) => Ok(Bool),
                    (left, right) => Err((format!("Invalid operation {op} for {left} and {right}"), *loc)),
                },
                And | Or => match (left.type_check(envir)?, right.type_check(envir)?) {
                    (Bool, Bool) => Ok(Bool),
                    (left, right) => Err((format!("Invalid operation {op} for {left} and {right}"), *loc)),
                },
                Assign => match (left.as_ref(), right.type_check(envir)?) {
                    (VarExp(id, loc), value) => {
                        let typ = match envir.lookup_var(id) {
                            Ok(typ) => typ,
                            Err(_) => return Err((format!("Variable {id} does not exist here"), *loc))
                        };
                        if typ != value {
                            Err((format!("Cannot assign {value} to {id} which is {typ}"), *loc))
                        } else {
                            Ok(Unit)
                        }
                    },
                    _ => unreachable!("Not a variable expression")
                },
                PlusAssign | MinusAssign => match (left.as_ref(), right.type_check(envir)?) {
                    (VarExp(id, _), value) => {
                        let typ = match envir.lookup_var(id) {
                            Ok(typ) => typ,
                            Err(_) => return Err((format!("Variable {id} does not exist here"), *loc))
                        };
                        match (typ, value) {
                            (Int, Int) => {},
                            (Float, Float) => {},
                            _ => return Err((format!("Cannot add {value} to {id} because it is {typ}"), *loc)),
                        };
                        Ok(Unit)
                    },
                    _ => unreachable!("Not a variable expression")
                }
                Not => unreachable!("Not a binary operator"),
            },
            UnOpExp(op, exp, loc) => match op {
                Minus => match exp.type_check(envir)? {
                    Int => Ok(Int),
                    Float => Ok(Float),
                    typ => Err((format!("Unary operator {op} is not valid for {typ}"), *loc)),
                },
                Not => match exp.type_check(envir)? {
                    Bool => Ok(Bool),
                    typ => Err((format!("Unary operator {op} is not valid for {typ}"), *loc)),
                },
                _ => unreachable!("Not a unary operator")
            },
            LiteralExp(lit, _) => {
                match lit {
                    Literal::Int(_) => Ok(Int),
                    Literal::Float(_) => Ok(Float),
                    Literal::Bool(_) => Ok(Bool),
                    Literal::Unit => unreachable!("Unit should not show up as a literal outside of returns"),
                }
            },
            BlockExp(exps, funs, _) => {
                //Type check of funs
                envir.enter_scope();
                for i in 0..funs.len() {
                    envir.push_function(funs[i].0.clone(), funs[i].1.clone());
                }
                for i in 0..funs.len() {
                    funs[i].1.type_check(envir)?;
                }
                envir.leave_scope();

                //Type check of block
                envir.enter_scope();
                for i in 0..funs.len() {
                    envir.push_function(funs[i].0.clone(), funs[i].1.clone());
                }

                let mut returned: Type = Unit;
                for exp in exps {
                    returned = exp.type_check(envir)?;
                }

                envir.leave_scope();

                Ok(returned)
            },
            VarExp(id, _) => {
                match envir.lookup_var(&id) {
                    Ok(typ) => Ok(typ),
                    Err(_) => todo!(),
                }
            },
            LetExp(id, exp, _) => {
                let value = exp.type_check(envir)?;
                envir.push_variable(id.clone(), value); 
                Ok(Unit)
            },
            IfElseExp(cond, pos, neg, loc) => {
                let cond = cond.type_check(envir)?;
                if cond != Bool {
                    return Err((format!("Condition for if must be boolean, got {cond}"), *loc))
                }
                if let Some(neg) = neg {
                    let pos_type = pos.type_check(envir)?;
                    let neg_type = neg.type_check(envir)?;
                    if pos_type != neg_type {
                        return Err((format!("If and else branch must have same type, got {pos_type} and {neg_type}"), *loc))
                    }
                    Ok(pos_type)
                } else {
                    Ok(Unit)
                }
            },
            WhileExp(cond, _, loc) => {
                if cond.type_check(envir)? != Bool {
                    return Err((format!("Condition for while must be boolean, got {cond}"), *loc))
                }
                Ok(Unit)
            }
            FunCallExp(id, args, loc) => {
                let func = match envir.lookup_fun(id) {
                    Ok(fun) => fun,
                    Err(_) => return Err((format!("Function {id} does not exist here"), *loc))
                };

                if func.ret_type == Any {
                    panic!("Recursive functions need type annotations")
                }
                else if args.len() != func.param_types.len() {
                    panic!("Incorrect arg count")
                }
                for i in 0..args.len() {
                    if args[i].type_check(envir)? != func.param_types[i] {
                        panic!("Incorrect arg type")
                    }
                }

                Ok(func.ret_type)
            },
        }
    }
}

impl Function {
    pub fn type_check(&mut self, envir: &mut TypeEnvironment) -> TypeResult {
        if self.ret_type == Unit {
            return Ok(Unit)
        }
        
        envir.enter_scope();

        for i in 0..self.param_types.len() {
            envir.push_variable(self.params[i].clone(), self.param_types[i].clone());
        }

        let res = self.exp.type_check(envir)?;

        envir.leave_scope();

        if self.ret_type == Any {
            self.ret_type = res
        } else if self.ret_type != res {
            return Err((format!("Return type does not match annotation, got {res} and {} was annotated", self.ret_type), self.loc))
        }

        Ok(res)
    }
}