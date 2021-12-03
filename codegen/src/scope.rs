use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A RegisterScope is used by the codegen crate to keep track of local variables/registers.
/// There can be a maximum of 255 registers active at once.
/// Register 0 is reserved for other usage.
pub struct RegisterScope<'a> {
    last_id: u8,
    parent: Option<Rc<RefCell<Self>>>,
    registers: HashMap<&'a str, u8>,
}

impl<'a> RegisterScope<'a> {
    pub fn create_child(parent: Rc<RefCell<Self>>) -> Self {
        let id = parent.borrow().last_id;
        Self {
            last_id: id,
            parent: Some(parent),
            registers: HashMap::new(),
        }
    }

    /// Gets the amount of registers in this scope (does NOT include parents)
    pub fn register_count(&self) -> u8 {
        // it is impossible for there to be more than 255 active registers
        self.registers.len() as u8
    }

    /// Creates a new register in this scope.
    pub fn register(&mut self, register: &'a str) -> Option<u8> {
        // TODO: Register overflow?
        self.last_id = self.last_id.checked_add(1)?;
        self.registers.insert(register, self.last_id);
        Some(self.last_id)
    }

    /// Finds a register in either this scope or its parents.
    pub fn find(&self, register: &'a str) -> Option<u8> {
        self.registers.get(register).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.borrow().find(register))
        })
    }
}

impl<'a> Default for RegisterScope<'a> {
    fn default() -> Self {
        Self {
            last_id: 0,
            parent: None,
            registers: HashMap::new(),
        }
    }
}
