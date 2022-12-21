use std::{rc::Rc, cell::RefCell};
use super::*;

pub struct EnvNode<T> {
    id: String,
    value: T,
    next: Option<Rc<RefCell<EnvNode<T>>>>,
}

impl<T: Clone> EnvNode<T> {
    pub fn new(id: String, value: T, next: Option<Rc<RefCell<EnvNode<T>>>>) -> Self {
        Self { id, value, next }
    }

    pub fn lookup(&self, id: &String) -> Result<T, String> {
        if self.id == *id {
            return Ok(self.value.clone())
        }

        if let Some(next) = &self.next {
            next.as_ref().borrow().lookup(id)
        } else {
            Err(format!("Id {id} not found"))
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
}

pub struct Environment<T> {
    var_head: Option<Rc<RefCell<EnvNode<T>>>>,
    var_scope_sizes: Vec<u16>,
    var_current_scope: u16,

    fun_head: Option<Rc<RefCell<EnvNode<Box<Function>>>>>,
    fun_scope_sizes: Vec<u16>,
    fun_current_scope: u16
}

impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Self { 
            var_head: None,
            var_scope_sizes: Vec::new(), 
            var_current_scope: 0, 
            fun_head: None, 
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
            self.var_head = self.var_head.take();
        }

        //Pop funs
        for _ in 0..self.fun_current_scope {
            self.fun_head = self.fun_head.take();
        }

        //Pop scope sizes
        self.var_current_scope = self.var_scope_sizes.pop().expect("TYPES Var stack was empty. Should never happen");
        self.fun_current_scope = self.fun_scope_sizes.pop().expect("TYPES Fun stack was empty. Should never happen");
    }

    pub fn push_variable(&mut self, id: String, value: T) {
        let new_var = EnvNode::new(id, value, self.var_head.take());
        self.var_head = Some(Rc::new(RefCell::new(new_var)));
        self.var_current_scope += 1;
    }

    pub fn push_function(&mut self, id: String, value: Box<Function>) {
        let new_fun = EnvNode::new(id, value, self.fun_head.take());
        self.fun_head = Some(Rc::new(RefCell::new(new_fun)));
        self.fun_current_scope += 1;
    }

    
    pub fn lookup_var(&self, id: &String) -> Result<T, String> {
        match &self.var_head {
            Some(head) => {
                head.borrow().lookup(id)
            },
            None => Err(format!("Variable not found")),
        }
    }

    pub fn lookup_fun(&self, id: &String) -> Result<Box<Function>, String> {
        match &self.fun_head {
            Some(head) => {
                head.borrow().lookup(id)
            },
            None => Err(format!("Variable not found")),
        }
    }

    pub fn mutate(&mut self, id: &String, value: T) {
        if let Some(head) = &self.var_head {
            head.as_ref().borrow_mut().mutate(id, value);
        }
    }
}

impl<T> Clone for Environment<T> {
    fn clone(&self) -> Self {
        Self {
            var_head: self.var_head.clone(), 
            var_scope_sizes: self.var_scope_sizes.clone(), 
            var_current_scope: self.var_current_scope, 
            fun_head: self.fun_head.clone(), 
            fun_scope_sizes: self.fun_scope_sizes.clone(),
            fun_current_scope: self.fun_current_scope
        }
    }
}

mod environment_tests {
    use super::*;

    #[test]
    fn test1() {
        //let mut envir = 
        //assert_eq!(x + y, 15);
    }
}