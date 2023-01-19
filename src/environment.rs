use std::{rc::Rc, cell::RefCell, fmt::Display};
use crate::definitions::*;

pub type FunStore = Rc<RefCell<Vec<Rc<RefCell<Box<Function>>>>>>;
type NodeRef<T> = Rc<RefCell<EnvNode<T>>>;

#[derive(Debug, Clone)]
pub enum Value<T: Clone + Display> {
    Var(T),
    Fun(Closure<T>), //contains fun store index
}

impl<T: Clone + Display> Display for Value<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Var(a) => a.fmt(f),
            Value::Fun(b) => b.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct EnvNode<T: Clone + Display> {
    scope_depth: u32,
    id: String,
    value: Value<T>,
    next: Option<NodeRef<T>>
}

#[derive(Debug)]
pub struct Environment<T: Clone + Display> {
    pub scope_depth: u32,
    head: Option<NodeRef<T>>,
    fun_store: FunStore,
}

impl<T: Clone + Display> EnvNode<T> {
    pub fn new(id: String, value: Value<T>, next: Option<NodeRef<T>>, scope_depth: u32) -> Self {
        Self { id, value, next, scope_depth }
    }
    
    pub fn lookup(&self, id: &str) -> Result<Value<T>, String> {
        if self.id == id {
            return Ok(self.value.clone())
        }

        if let Some(next) = &self.next {
            next.borrow().lookup(id)
        } else {
            Err(format!("Id '{id}' not found"))
        }
    }

    pub fn mutate(&mut self, id: &str, value: Value<T>) {
        if self.id == id {
            self.value = value;
            return;
        }

        if let Some(next) = &self.next {
            next.borrow_mut().mutate(id, value)
        } else {
            panic!("Mutate failed as id did not exist. Should never happen after correct type check")
        }
    }

    ///Gets the last node at the given scope depth
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

    ///Determines whether the id exists in the current scope
    pub fn id_exist_in_scope(&self, id: &str, scope: u32) -> bool {
        if self.scope_depth != scope {
            false
        } else if self.id == id {
            true
        } else if let Some(next) = &self.next {
            next.borrow().id_exist_in_scope(id, scope)
        } else {
            false
        }
    }

    pub fn init_fun_envirs(&mut self, scope: u32, envir: Environment<T>) {
        if self.scope_depth == scope {
            if let Value::Fun(clo) = &self.value {
                //Not optimal as it creates a copy TODO
                let mut clo = clo.clone();
                clo.set_envir(envir.clone());
                self.value = Value::Fun(clo)
            }

            if let Some(next) = &self.next {
                next.borrow_mut().init_fun_envirs(scope, envir)
            }
        } else {
            return;
        }
    }

    pub fn declare_fun(&mut self, id: &str, envir: Environment<T>) {
        if self.id == id {
            if let Value::Fun(clo) = &self.value {
                //Not optimal as it creates a copy TODO
                let mut clo = clo.clone();
                clo.set_envir(envir);
                clo.declared = true;
                self.value = Value::Fun(clo)
            }
            return;
        }

        if let Some(next) = &mut self.next {
            next.borrow_mut().declare_fun(id, envir)
        } else {
            panic!("Function did not exist. Should never happen after correct type check")
        }
    }
}

impl<T: Clone + Display + DeepCopy> Environment<T> {
    pub fn new(fun_store: FunStore) -> Self {
        Self { 
            scope_depth: 0,
            head: None,
            fun_store,
        }
    }

    pub fn enter_scope (&mut self) {
        self.scope_depth += 1;
    }

    pub fn leave_scope(&mut self) {
        self.scope_depth -= 1;
        self.head = match &self.head {
            Some(head) => if head.borrow().scope_depth > self.scope_depth {
                head.borrow().get_scope(self.scope_depth).clone()
            } else {
                Some(head.clone())
            },
            None => None,
        };
    }

    pub fn lookup_id(&self, id: &String) -> Result<Value<T>, String> {
        match &self.head {
            Some(head) => {
                head.borrow().lookup(id)
            },
            None => Err(format!("Variable '{id}' not found")),
        }
    }

    pub fn get_fun(&mut self, index: usize) -> Rc<RefCell<Box<Function>>> {
        self.fun_store.borrow()[index].clone()
    }

    pub fn push_variable(&mut self, id: &String, value: Value<T>) {
        let mut value = value;
        if let Value::Var(lit) = value {
            value = Value::Var(lit.deep_copy())
        }
        let new_var = EnvNode::new(id.clone(), value, self.head.clone(), self.scope_depth);
        self.head = Some(Rc::new(RefCell::new(new_var)));
    }

    pub fn id_exist_in_scope(&self, id: &String) -> bool {
        match &self.head {
            Some(head) => {
                head.borrow().id_exist_in_scope(id, self.scope_depth)
            },
            None => false,
        }
    }

    pub fn mutate(&mut self, id: &String, value: Value<T>) {
        let mut value = value;
        if let Value::Var(lit) = value {
            value = Value::Var(lit.deep_copy())
        }
        if let Some(head) = &self.head {
            head.as_ref().borrow_mut().mutate(id, value);
        }
    }

    ///Enables recursion by updating all scope functions with environment containing all other
    pub fn init_fun_envirs(&mut self) {
        if let Some(head) = &self.head {
            head.borrow_mut().init_fun_envirs(self.scope_depth, self.clone())
        }
    }

    ///Prepares the function for calls by setting environment to current
    pub fn declare_fun(&mut self, id: &String) {
        //Find the function closure and update the variable closure for declaration point
        if let Some(head) = &self.head {
            head.borrow_mut().declare_fun(&id, self.clone())
        }
    }

    pub fn get_scope(&mut self, scope: u32) -> Self {
        let head = match &self.head {
            Some(head) => if head.borrow().scope_depth > scope {
                head.borrow().get_scope(scope).clone()
            } else {
                Some(head.clone())
            },
            None => None,
        };
        Self { 
            scope_depth: scope,
            head,
            fun_store: self.fun_store.clone()
        }
    }
}

impl<T: Clone + Display> Clone for Environment<T> {
    fn clone(&self) -> Self {
        Self {
            scope_depth: self.scope_depth, 
            head: self.head.clone(),
            fun_store: self.fun_store.clone()
        }
    }
}