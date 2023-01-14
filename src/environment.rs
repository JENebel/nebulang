use core::panic;
use std::{rc::Rc, cell::RefCell};
use super::*;

#[derive(Debug)]
pub enum Variable<T> {
    Val(T),
    Fun(usize), //contains fun store index
}

#[derive(Debug)]
pub struct EnvNode<T> {
    scope_depth: u32,
    id: String,
    value: T,
    next: Option<Rc<RefCell<EnvNode<T>>>>,
}

#[derive(Debug)]
pub struct Environment<T> {
    pub scope_depth: u32,
    var_head: Option<Rc<RefCell<EnvNode<Variable<T>>>>>,
    fun_store: Rc<RefCell<Vec<Box<Function>>>>
}

impl<T: Clone> EnvNode<T> {
    pub fn new(id: String, value: T, next: Option<Rc<RefCell<EnvNode<T>>>>, scope_depth: u32) -> Self {
        Self { id, value, next, scope_depth }
    }

    pub fn lookup(&self, id: &String) -> Result<T, String> {
        if self.id == *id {
            return Ok(self.value.clone())
        }

        if let Some(next) = &self.next {
            next.borrow().lookup(id)
        } else {
            Err(format!("Id '{id}' not found"))
        }
    }

    pub fn id_exist_in_scope(&self, id: &String, scope: u32) -> bool {
        if self.scope_depth != scope {
            false
        } else if self.id == *id {
            true
        } else if let Some(next) = &self.next {
            next.borrow().id_exist_in_scope(id, scope)
        } else {
            false
        }
    }

    pub fn mutate(&mut self, id: &String, value: T) {
        if self.id == *id {
            self.value = value;
            return;
        }

        if let Some(next) = &mut self.next {
            next.borrow_mut().mutate(id, value)
        } else {
            panic!("Mutate failed as id did not exist. Should never happen after correct type check")
        }
    }
    
    pub fn get_scope(&self, scope: u32) -> Option<Rc<RefCell<EnvNode<T>>>> {
        if let Some(next) = &self.next {
            let next_scope = next.borrow().scope_depth;
            if next_scope > scope {
                next.borrow().get_scope(scope)
            } else {
                Some(next.clone())
            }
        } else {
            None
        }
    }
}

impl<'a, T: Clone> EnvNode<Closure<T>> {
    pub fn declare_fun(&mut self, id: &String, new_head: Option<Rc<RefCell<EnvNode<T>>>>) {
        if self.id == *id {
            self.value.envir.set_var_head(new_head);
            self.value.declared = true;
            return;
        }

        if let Some(next) = &mut self.next {
            next.borrow_mut().declare_fun(id, new_head)
        } else {
            panic!("Function did not exist. Should never happen after correct type check")
        }
    }

    pub fn update_fun_envir(&mut self, scope: u32, new_head: Rc<RefCell<EnvNode<Closure<T>>>>) {
        if self.scope_depth == scope {
            self.value.envir.set_fun_head(Some(new_head.clone()));
            if let Some(next) = &self.next {
                next.borrow_mut().update_fun_envir(scope, new_head)
            }
        } else {
            return;
        }
    }

    pub fn update_return_type(&mut self, id: &String, ret_type: Type) {
        if self.id == *id {
            self.value.fun.ret_type = ret_type;
        } else {
            if let Some(next) = &self.next {
                next.borrow_mut().update_return_type(id, ret_type)
            } else {
                panic!("Will only be called on existing functions")
            }
        }
    }
}

impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Self { 
            scope_depth: 0,
            var_head: None,
            fun_head: None,
        }
    }

    pub fn enter_scope (&mut self) {
        self.scope_depth += 1;
    }

    pub fn leave_scope(&mut self) {
        self.scope_depth -= 1;
        self.var_head = match &self.var_head {
            Some(head) => if head.borrow().scope_depth > self.scope_depth {
                head.borrow().get_scope(self.scope_depth).clone()
            } else {
                Some(head.clone())
            },
            None => None,
        };
        self.fun_head = match &self.fun_head {
            Some(head) => if head.borrow().scope_depth > self.scope_depth {
                head.borrow().get_scope(self.scope_depth).clone()
            } else {
                Some(head.clone())
            },
            None => None,
        };
    }

    pub fn push_variable(&mut self, id: String, value: T) {
        let new_var = EnvNode::new(id, value, self.var_head.take(), self.scope_depth);
        self.var_head = Some(Rc::new(RefCell::new(new_var)));
    }

    pub fn push_function(&mut self, id: String, fun: Box<Function>) {
        let new_fun = EnvNode::new(id, Closure::new(fun.clone(), self.clone()), self.fun_head.clone(), self.scope_depth);
        self.fun_head = Some(Rc::new(RefCell::new(new_fun)));
    }
    
    pub fn lookup_var(&self, id: &String) -> Result<T, String> {
        match &self.var_head {
            Some(head) => {
                head.borrow().lookup(id)
            },
            None => Err(format!("Variable '{id}' not found")),
        }
    }

    pub fn lookup_fun(&self, id: &String) -> Result<Closure<T>, String> {
        match &self.fun_head {
            Some(head) => {
                head.borrow().lookup(id)
            },
            None => Err(format!("Function '{id}' not found")),
        }
    }

    pub fn var_exist_in_scope(&self, id: &String) -> bool {
        match &self.var_head {
            Some(head) => {
                head.borrow().id_exist_in_scope(id, self.scope_depth)
            },
            None => false,
        }
    }

    pub fn fun_exist_in_scope(&self, id: &String) -> bool {
        match &self.fun_head {
            Some(head) => {
                head.borrow().id_exist_in_scope(id, self.scope_depth)
            },
            None => false,
        }
    }

    pub fn mutate(&mut self, id: &String, value: T) {
        if let Some(head) = &self.var_head {
            head.as_ref().borrow_mut().mutate(id, value);
        }
    }

    ///Enables recursion by updating all scope functions with environment containing all other
    pub fn update_fun_envirs(&mut self) {
        if let Some(head) = &self.fun_head {
            head.borrow_mut().update_fun_envir(self.scope_depth, head.clone())
        }
    }

    ///Prepares the function for calls
    pub fn declare_fun(&mut self, id: &String) {
        //Find the function closure and update the variable closure for declaration point
        if let Some(head) = &self.fun_head {
            head.borrow_mut().declare_fun(&id, self.var_head.clone())
        }
    }

    pub fn set_var_head(&mut self, new_head: Option<Rc<RefCell<EnvNode<T>>>>) {
        self.var_head = new_head
    }

    pub fn set_fun_head(&mut self, new_head: Option<Rc<RefCell<EnvNode<Closure<T>>>>>) {
        self.fun_head = new_head
    }

    pub fn update_return_type(&mut self, id: &String, ret_type: Type) {
        if let Some(head) = &self.fun_head {
            head.borrow_mut().update_return_type(&id, ret_type)
        } else {
            panic!("Should not be empty here!")
        }
    }

    pub fn get_scope(&mut self, scope: u32) -> Self {
        let var_head = match &self.var_head {
            Some(head) => if head.borrow().scope_depth > scope {
                head.borrow().get_scope(scope).clone()
            } else {
                Some(head.clone())
            },
            None => None,
        };
        let fun_head = match &self.fun_head {
            Some(head) => if head.borrow().scope_depth > scope {
                head.borrow().get_scope(scope).clone()
            } else {
                Some(head.clone())
            },
            None => None,
        };
        Self { 
            scope_depth: scope,
            var_head,
            fun_head
        }
    }
}

impl<T> Clone for Environment<T> {
    fn clone(&self) -> Self {
        Self {
            scope_depth: self.scope_depth, 
            var_head: self.var_head.clone(), 
            fun_head: self.fun_head.clone(), 
        }
    }
}

mod environment_tests {
    //use super::*;

    #[test]
    fn test1() {
        //let mut envir = 
        //assert_eq!(x + y, 15);
    }
}