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

    pub fn enter_scope (&mut self, funs: &Vec<(String, Box<Function>)>) {
        self.var_scope_sizes.push(self.var_current_scope);
        self.fun_scope_sizes.push(self.fun_current_scope);

        self.var_current_scope = 0;
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
            //self.fun_stack.pop();
        }
        
        //Pop scope size
        self.var_current_scope = self.var_scope_sizes.pop().expect("TYPES Var stack was empty. Should never happen");
        self.fun_current_scope = self.fun_scope_sizes.pop().expect("TYPES Fun stack was empty. Should never happen");
    }

    pub fn push_variable(&mut self, id: &String, value: Type) {
        self.var_current_scope += 1;
        self.var_stack.push((id.clone(), value));
    }

    pub fn lookup_var(&self, id: &String) -> &Type {
        &self.var_stack.iter().rev().find(|var| &var.0 == id).expect("TYPES Var id not found. Should never happen").1
    }

    pub fn lookup_fun(&self, id: &String) -> &Function {
        &self.fun_stack.iter().rev().find(|var| var.0 == *id).expect(format!("TYPES Fun id '{}' not found. Should never happen", id).as_str()).1
    }
}

impl<'a> Exp {
    pub fn type_check(&'a mut self, envir: &'a mut TypeEnvironment) -> Type {
        match self {
            BinOpExp(left, op, right, _) => match op {
                Plus | Minus | Multiply | Divide | Modulo => match (left.type_check(envir), right.type_check(envir)) {
                    (Int, Int) => Int,
                    (Int, Float) => Float,
                    (Float, Int) => Float,
                    (Float, Float) => Float,
                    _ => unreachable!("TYPE ERROR"),
                },
                LessThan | GreaterThan | LessOrEquals | GreaterOrEquals => match (left.type_check(envir), right.type_check(envir)) {
                    (Int, Int) => Bool,
                    (Int, Float) => Bool,
                    (Float, Int) => Bool,
                    (Float, Float) => Bool,
                    _ => unreachable!("TYPE ERROR"),
                },
                Equals | NotEquals => match (left.type_check(envir), right.type_check(envir)) {
                    (Int, Int) => Bool,
                    (Float, Float) => Bool,
                    (Bool, Bool) => Bool,
                    _ => unreachable!("Does not make sense"),
                },
                And | Or => match (left.type_check(envir), right.type_check(envir)) {
                    (Bool, Bool) => Bool,
                    _ => unreachable!("TYPE ERROR"),
                },
                Assign => match (left.as_ref(), right.type_check(envir)) {
                    (VarExp(id, _), value) => {
                        let typ = envir.lookup_var(id);
                        if *typ != value {
                            todo!("TYPE ERROR")
                        }
                        Unit
                    },
                    _ => unreachable!("Not a variable id")
                },
                PlusAssign | MinusAssign => match (left.as_ref(), right.type_check(envir)) {
                    (VarExp(id, _), value) => {
                        match (envir.lookup_var(id), value) {
                            (Int, Int) => {},
                            (Float, Float) => {},
                            _ => panic!("Invalid types")
                        };
                        Unit
                    },
                    _ => unreachable!("Not a variable id")
                },
                _ => unreachable!("Not a binary operator: '{op}'")
            },
            UnOpExp(op, exp, _) => match op {
                Minus => match exp.type_check(envir) {
                    Int => Int,
                    _ => unreachable!("TYPE ERROR"),
                },
                Not => match exp.type_check(envir) {
                    Bool => Bool,
                    _ => unreachable!("TYPE ERROR"),
                },
                _ => unreachable!("Not a unary operator")
            },
            LiteralExp(lit, _) => {
                match lit {
                    Literal::Int(_) => Int,
                    Literal::Float(_) => Float,
                    Literal::Bool(_) => Bool,
                    Literal::Unit => unreachable!(),
                }
            },
            BlockExp(exps, funs, _) => {
                envir.enter_scope(&funs);

                for i in 0..funs.len() {
                    if funs[i].1.is_checked { break }
                    funs[i].1.type_check(envir);
                }
                
                let mut returned = Unit;
                for exp in exps {
                    returned = exp.type_check(envir);
                }

                envir.leave_scope();

                returned
            },
            VarExp(id, _) => envir.lookup_var(&id).clone(),
            LetExp(id, exp, _) => {
                let value = exp.type_check(envir);
                envir.push_variable(&id, value); 
                Unit
            },
            IfElseExp(cond, pos, neg, _) => {
                let cond = cond.type_check(envir);
                if cond != Bool {panic!("Condition must be boolean")}
                if let Some(neg) = neg {
                    let pos_type = pos.type_check(envir);
                    let neg_type = neg.type_check(envir);

                    if pos_type != neg_type {panic!("Both branches should have same type")}

                    pos_type
                } else {
                    Unit
                }
            },
            WhileExp(cond, _, _) => {
                if cond.type_check(envir) != Bool {
                    panic!("Condition should be bool")
                }

                Unit
            }
            FunCallExp(id, args, _) => {
                let func = envir.lookup_fun(id).clone();

                if func.ret_type == Any {
                    panic!("Recursive functions need type annotations")
                }
                else if args.len() != func.param_types.len() {
                    panic!("Incorrect arg count")
                }
                for i in 0..args.len() {
                    if args[i].type_check(envir) != func.param_types[i] {
                        panic!("Incorrect arg type")
                    }
                }

                func.ret_type
            },
        }
    }
}

impl Function {
    pub fn type_check(&mut self, envir: &mut TypeEnvironment) -> Type {
        if self.ret_type == Unit {
            return Unit
        }
        
        envir.enter_scope(&Vec::new());

        for i in 0..self.param_types.len() {
            envir.push_variable(&self.params[i], self.param_types[i].clone());
        }

        let res = self.exp.type_check(envir);

        envir.leave_scope();

        if self.ret_type == Any {
            self.ret_type = res
        } else if self.ret_type != res {
            panic!("Type does not match annotation")
        }

        self.is_checked = true;

        res
    }
}