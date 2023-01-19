use crate::definitions::*;
use crate::lexer::*;
use crate::environment::*;

use Type::*;
use Operator::*;
use Exp::*;
use ErrorType::*;

type TypeResult = Result<Type, Error>;

pub struct TypeContext {
    pub expected_type: Type,
    pub is_in_loop: bool,
}

impl TypeContext {
    pub fn new() -> Self {
        Self { expected_type: Any, is_in_loop: false }
    }
}

impl<'a> Exp {
    pub fn type_check(&'a self, envir: &'a mut Environment<Type>, context: &mut TypeContext) -> TypeResult {
        match self {
            BinOpExp(left, op, right, loc) => match op {
                Plus => match (left.type_check(envir, context)?, right.type_check(envir, context)?) {
                    (Int, Int) => Ok(Int),
                    (Int, Float) => Ok(Float),
                    (Float, Int) => Ok(Float),
                    (Float, Float) => Ok(Float),

                    //String
                    (Str, Str) | (Str, Char) | (Char, Str)
                    | (Str, Int) | (Int, Str) | (Str, Float)
                    | (Float, Str) | (Str, Bool) | (Bool, Str)
                    | (Char, Char) => Ok(Str),

                    (left, right) => Err(Error::new(TypeError, format!("Invalid operation '{op}' for '{left}' and '{right}'."), *loc)),
                },
                Minus | Multiply | Divide | Modulo => match (left.type_check(envir, context)?, right.type_check(envir, context)?) {
                    (Int, Int) => Ok(Int),
                    (Int, Float) => Ok(Float),
                    (Float, Int) => Ok(Float),
                    (Float, Float) => Ok(Float),
                    (left, right) => Err(Error::new(TypeError, format!("Invalid operation '{op}' for '{left}' and '{right}'."), *loc)),
                },
                LessThan | GreaterThan | LessOrEquals | GreaterOrEquals => match (left.type_check(envir, context)?, right.type_check(envir, context)?) {
                    (Int, Int) => Ok(Bool),
                    (Int, Float) => Ok(Bool),
                    (Float, Int) => Ok(Bool),
                    (Float, Float) => Ok(Bool),
                    (left, right) => Err(Error::new(TypeError, format!("Invalid operation '{op}' for '{left}' and '{right}'."), *loc)),
                },
                Equals | NotEquals => match (left.type_check(envir, context)?, right.type_check(envir, context)?) {
                    (Int, Int) => Ok(Bool),
                    (Float, Float) => Ok(Bool),
                    (Bool, Bool) => Ok(Bool),
                    (left, right) => Err(Error::new(TypeError, format!("Invalid operation '{op}' for '{left}' and '{right}'."), *loc)),
                },
                And | Or => match (left.type_check(envir, context)?, right.type_check(envir, context)?) {
                    (Bool, Bool) => Ok(Bool),
                    (left, right) => Err(Error::new(TypeError, format!("Invalid operation '{op}' for '{left}' and '{right}'."), *loc)),
                },
                Assign => {
                    let value = right.type_check(envir, context)?;

                    match (left.as_ref(), value) {
                        (VarExp(id, loc), value) => {
                            let typ = match envir.lookup_id(id) {
                                Ok(Value::Var(typ)) => typ,
                                Ok(Value::Fun(_)) => return Err(Error::new(TypeError, format!("'{id}' was a function. TODO."), *loc)),
                                Err(_) => return Err(Error::new(TypeError, format!("Variable '{id}' does not exist here."), *loc))
                            };
                            if typ != value {
                                Err(Error::new(TypeError, format!("Cannot assign '{value}' to '{id}' which is '{typ}'."), *loc))
                            } else {
                                Ok(Unit)
                            }
                        },
                        (AccessArrayExp(_, _, loc), value) => {
                            let elem_type = left.type_check(envir, context)?;
                            if elem_type != value {
                                Err(Error::new(TypeError, format!("Cannot assign '{value}' to element in array of type: '{}'.", Type::Array(Box::new(elem_type))), *loc))
                            } else {
                                Ok(Unit)
                            }
                        },
                        _ => Err(Error::new(TypeError, format!("Left side of assignment must be a variable name."), *loc))
                    }
                },
                PlusAssign | MinusAssign | DivideAssign | MultiplyAssign => match left.as_ref() {
                    VarExp(_, loc) | AccessArrayExp(_, _, loc) => {
                        Exp::BinOpExp(left.clone(), op.strip_assign(), right.clone(), *loc).type_check(envir, context)?; //Should not use clone here TODO
                        Ok(Unit)
                    },
                    _ => unreachable!("Not a variable id.")
                },
                Not => unreachable!("Not a binary operator."),
            },
            UnOpExp(op, exp, loc) => match op {
                Minus => match exp.type_check(envir, context)? {
                    Int => Ok(Int),
                    Float => Ok(Float),
                    typ => Err(Error::new(TypeError, format!("Unary operator '{op}' is not valid for '{typ}'."), *loc)),
                },
                Not => match exp.type_check(envir, context)? {
                    Bool => Ok(Bool),
                    typ => Err(Error::new(TypeError, format!("Unary operator '{op}' is not valid for '{typ}'."), *loc)),
                },
                _ => unreachable!("Not a unary operator.")
            },
            LiteralExp(lit, _) => {
                Ok(lit.get_type())
            },
            BlockExp(exps, funs, loc) => {
                envir.enter_scope();

                for i in 0..funs.len() {
                    let id = &funs[i].0;

                    if envir.id_exist_in_scope(id) {
                        return Err(Error::new(TypeError, format!("Function '{}' already exist in this scope.", funs[i].0), *loc))
                    }

                    // It is a waste to initialize envir here, maybe fix some time TODO. Same in eval
                    envir.push_variable(id, Value::Fun(Closure::new(funs[i].1, envir.clone())));
                }

                envir.init_fun_envirs();

                let mut returned: Type = Unit;
                for exp in exps {
                    returned = exp.type_check(envir, context)?;
                }

                envir.leave_scope();

                Ok(returned)
            },
            VarExp(id, loc) => {
                match envir.lookup_id(&id) {
                    Ok(Value::Var(typ)) => Ok(typ),
                    Ok(Value::Fun(_)) => Err(Error::new(TypeError, format!("'{id}' is a function and can not be used like a variable."), *loc)),
                    Err(_) => Err(Error::new(TypeError, format!("Variable '{id}' does not exist here."), *loc)),
                }
            },
            LetExp(id, exp, loc) => {
                if envir.id_exist_in_scope(&id) {
                    return Err(Error::new(TypeError, format!("Variable '{id}' already exist in this scope."), *loc))
                }

                let value = exp.type_check(envir, context)?;

                if !value.is_value_type() {
                    return Err(Error::new(TypeError, format!("Cannot use '{value}' as a value."), *loc))
                }

                envir.push_variable(id, Value::Var(value)); 
                Ok(Unit)
            },
            IfElseExp(cond, pos, neg, loc) => {
                let cond = cond.type_check(envir, context)?;
                if cond != Bool {
                    return Err(Error::new(TypeError, format!("Condition for if must be boolean, got '{cond}'."), *loc))
                }
                let pos_type = pos.type_check(envir, context)?;
                if let Some(neg) = neg {
                    let neg_type = neg.type_check(envir, context)?;
                    if pos_type != neg_type {
                        return Err(Error::new(TypeError, format!("If and else branch must have same type, got '{pos_type}' and '{neg_type}'."), *loc))
                    }
                    Ok(pos_type)
                } else {
                    Ok(Unit)
                }
            },
            LoopExp(body, inc, _) => {
                // Check body with is_in_loop = true
                let in_loop_before = context.is_in_loop;
                context.is_in_loop = true;
                body.type_check(envir, context)?;
                context.is_in_loop = in_loop_before;

                if let Some(exp) = inc {
                    exp.type_check(envir, context)?;
                }

                Ok(Unit)
            },
            FunCallExp(id, args, loc) => {
                let closure = match envir.lookup_id(id) {
                    Ok(Value::Fun(clo)) => clo,
                    Ok(Value::Var(typ)) => return Err(Error::new(TypeError, format!("Cannot call '{id}' as a function. It has type '{typ}'."), *loc)),
                    Err(_) => return Err(Error::new(TypeError, format!("Function '{id}' does not exist here."), *loc))
                };

                let rc = envir.get_fun(closure.fun);
                let func = rc.borrow().clone();

                if func.ret_type.is_any() {
                    return Err(Error::new(TypeError, format!("Cannot call '{id}' here. '{id}' needs a type annotation as the call is prior to its definition."), *loc))
                }
                
                if args.len() != func.param_types.len() {
                    return Err(Error::new(TypeError, format!("Incorrect argument count. '{id}' takes {} arguments, but {} were given.", func.param_types.len(), args.len()), *loc))
                }

                for i in 0..args.len() {
                    let checked_type = args[i].type_check(envir, context)?;
                    if checked_type != func.param_types[i] {
                        return Err(Error::new(TypeError, format!("Incorrect argument type. Expected type '{}' for argument {}, but got {}.", func.param_types[i], func.params[i], checked_type), *loc))
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
            InitTemplateArrayExp(length_exp, template_exp, loc) => {
                let length_type = length_exp.type_check(envir, context)?;
                if length_type != Int {
                    return Err(Error::new(TypeError, format!("Array length must be 'int', found '{length_type}'."), *loc));
                }

                let element_type = match template_exp.type_check(envir, context)? {
                    Ref(typ) => return Err(Error::new(TypeError, format!("Array template cannot be a reference, found '{}'.", Ref(typ)), *loc)),
                    typ => typ
                };

                Ok(Array(Box::new(element_type)))
            },
            AccessArrayExp(array_exp, index_exp, loc) => {
                let elem_type = match array_exp.type_check(envir, context)? {
                    Type::Array(elem_type) => elem_type,
                    t => return Err(Error::new(TypeError, format!("Cannot acces '{t}' like an array."), *loc)),
                };

                match index_exp.type_check(envir, context)? {
                    Type::Int => { /* OK */ },
                    t => return Err(Error::new(TypeError, format!("Array index must be 'int', found '{t}'."), *loc)),
                }

                Ok(*elem_type)
            },
            InitArrayWithValuesExp(values, loc) => {
                let mut element_types: Vec<Type> = Vec::new();

                // Type check values
                for v in values {
                    element_types.push(v.type_check(envir, context)?)
                }

                // Check if all values are same type
                let element_type = element_types.first().unwrap();

                if !element_type.is_value_type() {
                    return Err(Error::new(TypeError, format!("Cannot use '{element_type}' as a value."), *loc))
                }

                if element_types.iter().any(|t| t != element_type) {
                    Err(Error::new(TypeError, format!("All elements of array must have same type, but got these {values:?}."), *loc))
                } else {
                    Ok(Array(Box::new(element_type.clone())))
                }
            },
            ReturnExp(exp, loc) => {
                let return_type = exp.type_check(envir, context)?;

                if context.expected_type.is_any() {
                    context.expected_type = return_type;
                } else if return_type != context.expected_type {
                    return Err(Error::new(TypeError, format!("Incorrect return type. Expected '{}', but got '{return_type}'", context.expected_type), *loc))
                }
                Ok(Any)
            },
            BreakExp(loc) | ContinueExp(loc) => {
                if !context.is_in_loop {
                    return Err(Error::new(TypeError, format!("Cannot use '{self}' outside of a loop."), *loc))
                }
                
                Ok(Any)
            },
        }
    }
}

impl<'a> Function {
    pub fn type_check(&self, id: &str, loc: Location, envir: &mut Environment<Type>,) -> TypeResult {
        envir.enter_scope();

        for i in 0..self.param_types.len() {
            envir.push_variable(&self.params[i].clone(), Value::Var(self.param_types[i].clone()));
        }

        let context = &mut TypeContext::new();
        context.expected_type = self.ret_type.clone();

        let res = self.exp.type_check(envir, context)?;

        envir.leave_scope();

        if self.annotated && self.ret_type != res {
            return Err(Error::new(TypeError, format!("Return type for '{id}' does not match annotation, got '{res}' but '{}' was annotated.", self.ret_type), loc))
        } else if context.expected_type != res {
            return Err(Error::new(TypeError, format!("Return type mismatch in '{id}'. Expected '{res}' but '{}' is returned within the function.", context.expected_type), loc))
        }

        Ok(res)
    }
}