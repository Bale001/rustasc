use crate::scope::RegisterScope;
use indexmap::IndexSet;
use parser::ast::expression::{Compare, Expression, Literal, Side, Symbol};
use parser::ast::identifier::{Identifier, SymbolName};
use parser::ast::instruction::{Block, Instruction};
use parser::ast::operation::Operation;
use parser::ast::package::Script as AstScript;
use parser::ast::statements::{Declare, For, ForIn, If, Switch, Try, While, With};
use std::cell::RefCell;
use std::io::Result;
use std::ops::Deref;
use std::rc::Rc;
use swf::avm1::types::{Action, FunctionFlags, TryFlags, Value};
use swf::avm1::write::Writer;
use swf::write::SwfWriteExt;
use swf::SwfStr;

use std::borrow::Cow;

const SWF_VERSION: u8 = 8;

pub struct Script<'a> {
    output: Vec<u8>,
    constant_pool: IndexSet<Cow<'a, str>>,
}

pub struct ActionBlock<'a, 'b> {
    output: Vec<u8>,
    stack: Vec<Value<'a>>,
    lazy_resolve: Vec<LazyResolve>,
    constant_pool: Option<&'b mut IndexSet<Cow<'a, str>>>,
    registers: Option<Rc<RefCell<RegisterScope<'a>>>>,
}

#[derive(Clone, Copy)]
pub enum ResolveType {
    Break,
    Continue,
}

#[derive(Clone, Copy)]
pub struct LazyResolve {
    kind: ResolveType,
    address: usize,
}

impl<'a> Default for Script<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Script<'a> {
    pub fn new() -> Self {
        Self {
            output: Vec::new(),
            constant_pool: IndexSet::new(),
        }
    }

    pub fn write_script(&mut self, script: AstScript<'a>) -> Result<()> {
        let block_bytes = {
            let mut action_block = ActionBlock::new(Some(&mut self.constant_pool), None);
            for (name, function) in script.definitions().iter() {
                if let Symbol::Function(params, code, _, flags) = function {
                    action_block.write_function(Some(name.base()), params, code, *flags)?;
                }
            }
            action_block.write_block(script.main())?;
            action_block.into_bytes()
        };
        let mut writer = Writer::new(&mut self.output, SWF_VERSION);
        if !self.constant_pool.is_empty() {
            // NOTE: We need to write the ConstantPool instruction manually
            // because Action::ConstantPool only works with Vec's, but RustASC uses
            // an IndexSet to keep track of constants.
            let len = 2 + self
                .constant_pool
                .iter()
                .map(|c| c.len() + 1)
                .sum::<usize>();
            writer.write_opcode_and_length(0x88, len)?;
            writer.write_u16(self.constant_pool.len() as u16)?;
            for constant in self.constant_pool.iter() {
                writer.write_string(SwfStr::from_utf8_str(constant))?;
            }
        }
        self.output.extend_from_slice(&block_bytes);
        Ok(())
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.output
    }
}

impl<'a, 'b> ActionBlock<'a, 'b> {
    pub fn new(
        constant_pool: Option<&'b mut IndexSet<Cow<'a, str>>>,
        parent: Option<Rc<RefCell<RegisterScope<'a>>>>,
    ) -> Self {
        Self {
            output: Vec::new(),
            stack: Vec::new(),
            lazy_resolve: Vec::new(),
            constant_pool,
            registers: parent,
        }
    }

    pub fn into_parts(self) -> (Vec<u8>, Vec<LazyResolve>) {
        (self.output, self.lazy_resolve)
    }

    pub fn from_parts(
        constant_pool: Option<&'b mut IndexSet<Cow<'a, str>>>,
        output: Vec<u8>,
        lazy_resolve: Vec<LazyResolve>,
    ) -> Self {
        Self {
            output,
            stack: Vec::new(),
            lazy_resolve,
            constant_pool,
            registers: None,
        }
    }

    pub fn add_breakpoint_addresses(&mut self, addresses: &[LazyResolve]) {
        for lazy in addresses {
            self.lazy_resolve.push(LazyResolve {
                kind: lazy.kind,
                address: lazy.address + self.output.len(),
            });
        }
    }

    pub fn fix_breakpoint_addresses(&self, addresses: &mut [usize]) {
        for address in addresses {
            *address += self.output.len()
        }
    }

    pub fn len(&self) -> usize {
        self.output.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn write_block(&mut self, block: &Block<'a>) -> Result<()> {
        for instruction in block.code() {
            self.write_instruction(instruction)?;
        }
        Ok(())
    }

    /// Writes an action to the output
    pub fn write_action(&mut self, action: Action<'a>) -> Result<()> {
        let mut writer = Writer::new(&mut self.output, SWF_VERSION);
        writer.write_action(&action)
    }

    pub fn flush(&mut self) -> Result<()> {
        if !self.stack.is_empty() {
            let stack = std::mem::take(&mut self.stack);
            let push = Action::Push(stack);
            self.write_action(push)?;
        }
        Ok(())
    }

    /// Same thing as `self.write_action`, but also flushes the stack.
    pub fn emit(&mut self, action: Action<'a>) -> Result<()> {
        self.flush()?;
        self.write_action(action)
    }

    pub fn push_cow_str(&mut self, string: Cow<'a, str>) -> Result<()> {
        if let Some(constant_pool) = self.constant_pool.as_mut() {
            let index = constant_pool.get_index_of(&string).or_else(|| {
                if constant_pool.len() < u16::MAX.into() {
                    let (index, _) = constant_pool.insert_full(string.clone());
                    Some(index)
                } else {
                    None
                }
            });
            if let Some(index) = index {
                self.stack.push(Value::ConstantPool(index as u16));
                return Ok(());
            }
        }
        self.flush()?;
        // Writing manually here for efficiency
        let mut writer = Writer::new(&mut self.output, SWF_VERSION);
        writer.write_opcode_and_length(0x96, string.len() + 2)?;
        writer.write_u8(0)?;
        writer.write_string(SwfStr::from_utf8_str(&string))
    }

    pub fn resolve_string(&mut self, string: &'a str) -> Value<'a> {
        if let Some(constant_index) = self
            .constant_pool
            .as_mut()
            .map(|pool| pool.insert_full(Cow::from(string)))
            .and_then(|(index, _)| u16::try_from(index).ok())
        {
            Value::ConstantPool(constant_index)
        } else {
            Value::Str(SwfStr::from_utf8_str(string))
        }
    }

    pub fn resolve_literal(&mut self, constant: Literal<'a>) -> Value<'a> {
        match constant {
            Literal::Undefined => Value::Undefined,
            Literal::Null => Value::Null,
            Literal::Bool(b) => Value::Bool(b),
            Literal::Integer(i) => match i.try_into().ok() {
                Some(i) => Value::Int(i),
                None => Value::Double(i as f64),
            },
            Literal::Number(n) => Value::Double(n),
            Literal::String(s) => self.resolve_string(s),
        }
    }

    /// Writes an expression.
    /// If `push` is true, it will push the return value on the stack.
    pub fn write_expression<'c>(
        &mut self,
        expression: &'c Expression<'a>,
        push: bool,
    ) -> Result<()> {
        let has_ternary = expression.ternary().is_some();
        let has_compare = !expression.compares().is_empty();
        if let Some(base) = expression.base() {
            self.write_symbol(base, push || has_ternary || has_compare)?;
        }
        for (operation, right) in expression.arithmetic() {
            let (action, invert) = match operation {
                Operation::Add => (Action::Add2, false),
                Operation::Sub => (Action::Subtract, false),
                Operation::Mul => (Action::Multiply, false),
                Operation::Div => (Action::Divide, false),
                Operation::Mod => (Action::Modulo, false),
                Operation::BitAnd => (Action::BitAnd, false),
                Operation::BitLShift => (Action::BitLShift, false),
                Operation::BitOr => (Action::BitOr, false),
                Operation::BitRShift => (Action::BitRShift, false),
                Operation::BitURShift => (Action::BitURShift, false),
                Operation::BitXor => (Action::BitXor, false),
                Operation::Greater => (Action::Greater, false),
                Operation::GreaterEqual => (Action::Less2, true),
                Operation::Less => (Action::Less2, false),
                Operation::LessEqual => (Action::Greater, true),
                Operation::Equality => (Action::Equals2, false),
                Operation::Inequality => (Action::Equals2, true),
                Operation::StrictEquality => (Action::StrictEquals, false),
                Operation::StrictInequality => (Action::StrictEquals, true),
                Operation::InstanceOf => (Action::InstanceOf, false),
            };
            self.write_symbol(right, push || has_ternary || has_compare)?;
            if push || has_ternary || has_compare {
                self.emit(action)?;
                if invert {
                    self.emit(Action::Not)?;
                }
            }
        }

        for (compare, expr) in expression.compares().iter() {
            let mut cond_block = crate::create_block!(self);
            cond_block.emit(Action::Pop)?;
            cond_block.write_expression(expr, true)?;
            cond_block.flush()?;
            let cond_bytes = cond_block.into_bytes();
            self.emit(Action::PushDuplicate)?;
            if matches!(compare, Compare::And) {
                self.emit(Action::Not)?;
            }
            self.emit(Action::If {
                offset: cond_bytes.len() as i16,
            })?;
            self.output.extend_from_slice(&cond_bytes);
            if !push && !has_ternary {
                self.emit(Action::Pop)?;
            }
        }
        if let Some((left, right)) = expression.ternary() {
            self.emit(Action::Not)?;
            let mut left_block = crate::create_block!(self);
            left_block.write_expression(left, true)?;
            left_block.flush()?;
            let left_bytes = left_block.into_bytes();
            self.emit(Action::If {
                offset: left_bytes.len() as i16 + 5,
            })?;
            self.output.extend_from_slice(&left_bytes);
            let mut right_block = crate::create_block!(self);
            right_block.write_expression(right, true)?;
            right_block.flush()?;
            let right_bytes = right_block.into_bytes();
            self.emit(Action::Jump {
                offset: right_bytes.len() as i16,
            })?;
            self.output.extend_from_slice(&right_bytes);
            if !push {
                self.emit(Action::Pop)?;
            }
        }

        Ok(())
    }

    pub fn write_index(
        &mut self,
        this: &Symbol<'a>,
        value: &Expression<'a>,
        get: bool,
    ) -> Result<()> {
        self.write_symbol(this, true)?;
        self.write_expression(value, true)?;
        if get {
            self.emit(Action::GetMember)?;
        }
        Ok(())
    }

    /// Writes a symbol.
    /// If `push` is true, it will push the return value on the stack.
    pub fn write_symbol(&mut self, symbol: &Symbol<'a>, push: bool) -> Result<()> {
        match symbol {
            Symbol::Expression(e) => self.write_expression(e, push),
            Symbol::Literal(literal) if push => {
                let resolved = self.resolve_literal(*literal);
                self.stack.push(resolved);
                Ok(())
            }
            Symbol::Identifier(id) if push => {
                self.write_identifier(id, true)?;
                Ok(())
            }
            Symbol::Index(index) => self.write_index(&index.0, &index.1, true),
            Symbol::Assign(assign) => {
                let is_member = match &assign.0 {
                    Symbol::Identifier(id) => {
                        if id.path().is_none() {
                            if let Some(id) = self
                                .registers
                                .as_mut()
                                .and_then(|registers| registers.borrow_mut().find(id.base()))
                            {
                                self.write_expression(&assign.1, true)?;
                                self.emit(Action::StoreRegister(id))?;
                                if !push {
                                    self.emit(Action::Pop)?;
                                }
                                return Ok(());
                            }
                        }
                        self.write_identifier(id, false)?
                    }
                    Symbol::Index(index) => {
                        self.write_index(&index.0, &index.1, false)?;
                        true
                    }
                    _ => unreachable!(),
                };
                self.write_expression(&assign.1, true)?;
                if push {
                    // If push is enabled, save the value in register 0 for later
                    self.emit(Action::StoreRegister(0))?;
                }
                if is_member {
                    self.emit(Action::SetMember)?;
                } else {
                    self.emit(Action::SetVariable)?;
                }
                if push {
                    // Now push the saved value on the stack
                    self.stack.push(Value::Register(0));
                }
                Ok(())
            }
            Symbol::Not(this) if push => {
                self.write_symbol(this, true)?;
                self.emit(Action::Not)
            }
            Symbol::BitNot(this) if push => {
                self.write_symbol(this, true)?;
                self.stack.push(Value::Double(u32::MAX as f64));
                self.emit(Action::BitXor)
            }
            Symbol::Typeof(this) if push => {
                self.write_symbol(this, true)?;
                self.emit(Action::TypeOf)
            }
            Symbol::Delete(this) => {
                let is_member = match this.deref() {
                    Symbol::Identifier(id) => self.write_identifier(id, false)?,
                    Symbol::Index(index) => {
                        self.write_index(&index.0, &index.1, false)?;
                        true
                    }
                    _ => unreachable!(),
                };
                if is_member {
                    self.emit(Action::Delete)?;
                } else {
                    self.emit(Action::Delete2)?;
                }
                if !push {
                    self.emit(Action::Pop)?;
                }
                Ok(())
            }
            Symbol::Call(call, new) => {
                if let Symbol::Identifier(id) = &call.0 {
                    match id.base() {
                        "trace" => {
                            if let Some(this) = call.1.first() {
                                self.write_expression(this, true)?;
                                return self.emit(Action::Trace);
                            }
                        }
                        "set" if call.1.len() == 2 => {
                            for param in call.1.iter() {
                                self.write_expression(param, true)?;
                            }
                            return self.emit(Action::SetVariable);
                        }
                        _ => (),
                    }
                }
                for param in call.1.iter().rev() {
                    self.write_expression(param, true)?;
                }
                self.stack.push(Value::Int(call.1.len() as i32));
                let is_member = match &call.0 {
                    Symbol::Identifier(id) => self.write_identifier(id, false)?,
                    Symbol::Index(index) => {
                        self.write_index(&index.0, &index.1, false)?;
                        true
                    }
                    s => {
                        self.write_symbol(s, true)?;
                        false
                    }
                };
                let action = new
                    .then(|| {
                        if is_member {
                            Action::NewMethod
                        } else {
                            Action::NewObject
                        }
                    })
                    .unwrap_or_else(|| {
                        if is_member {
                            Action::CallMethod
                        } else {
                            Action::CallFunction
                        }
                    });
                self.emit(action)?;
                if !push {
                    self.emit(Action::Pop)?;
                }
                Ok(())
            }
            Symbol::Array(array) if push => {
                for item in array.iter().rev() {
                    self.write_expression(item, true)?;
                }
                self.stack.push(Value::Int(array.len() as i32));
                self.emit(Action::InitArray)
            }
            Symbol::Object(obj) if push => {
                for (name, value) in obj {
                    let resolved = self.resolve_string(name);
                    self.stack.push(resolved);
                    self.write_expression(value, true)?;
                }
                self.stack.push(Value::Int(obj.len() as i32));
                self.emit(Action::InitObject)
            }
            Symbol::Function(params, code, _, flags) => {
                self.write_function(None, params, code, *flags)
            }
            s @ (Symbol::Incr(this, side) | Symbol::Decr(this, side)) => {
                let action = match s {
                    Symbol::Incr(_, _) => Action::Increment,
                    Symbol::Decr(_, _) => Action::Decrement,
                    _ => unreachable!(),
                };
                match side {
                    Side::Left => {
                        let is_member = match this.deref() {
                            Symbol::Identifier(id) => self.write_identifier(id, false)?,
                            Symbol::Index(index) => {
                                self.write_index(&index.0, &index.1, false)?;
                                false
                            }
                            _ => unreachable!(),
                        };
                        self.write_symbol(this, true)?;
                        self.emit(action)?;
                        if push {
                            self.emit(Action::StoreRegister(0))?;
                        }
                        if is_member {
                            self.emit(Action::SetMember)?;
                        } else {
                            self.emit(Action::SetVariable)?;
                        }
                        if push {
                            self.stack.push(Value::Register(0));
                        }
                    }
                    Side::Right => {
                        if push {
                            self.write_symbol(this, true)?;
                        }
                        let is_member = match this.deref() {
                            Symbol::Identifier(id) => self.write_identifier(id, false)?,
                            Symbol::Index(index) => {
                                self.write_index(&index.0, &index.1, false)?;
                                false
                            }
                            _ => unreachable!(),
                        };
                        self.write_symbol(this, true)?;
                        self.emit(action)?;
                        if is_member {
                            self.emit(Action::SetMember)?;
                        } else {
                            self.emit(Action::SetVariable)?;
                        }
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub fn write_function(
        &mut self,
        name: Option<&'a str>,
        params: &[SymbolName<'a>],
        code: &Block<'a>,
        flags: FunctionFlags,
    ) -> Result<()> {
        let scope = Rc::new(RefCell::new(RegisterScope::default()));
        self.flush()?;
        {
            let mut scope = scope.borrow_mut();
            if flags.contains(FunctionFlags::PRELOAD_THIS) {
                scope.register("this");
            }
            if flags.contains(FunctionFlags::PRELOAD_ARGUMENTS) {
                scope.register("arguments");
            }
            if flags.contains(FunctionFlags::PRELOAD_SUPER) {
                scope.register("super");
            }
            if flags.contains(FunctionFlags::PRELOAD_ROOT) {
                scope.register("_root");
            }
            if flags.contains(FunctionFlags::PRELOAD_PARENT) {
                scope.register("_parent");
            }
            if flags.contains(FunctionFlags::PRELOAD_GLOBAL) {
                scope.register("_global");
            }
        }
        let len = name.map(|name| name.len()).unwrap_or(0)
            + 4
            + params
                .iter()
                .map(|param| param.name().len() + 2)
                .sum::<usize>()
            + 4;
        let mut param_registers = Vec::with_capacity(params.len());
        for param in params {
            let register = scope
                .borrow_mut()
                .register(param.name())
                .expect("Register overflow");
            param_registers.push((register, param.name()))
        }
        let mut block = crate::create_block_with_scope!(self, scope.clone());
        block.write_block(code)?;
        let bytes = block.into_bytes();
        let mut writer = Writer::new(&mut self.output, SWF_VERSION);
        writer.write_opcode_and_length(0x8E, len)?;
        writer.write_string(SwfStr::from_utf8_str(name.unwrap_or("")))?;
        writer.write_u16(params.len() as u16)?;
        writer.write_u8(scope.borrow().register_count() + params.len() as u8)?;
        writer.write_u16(flags.bits())?;

        for (register, name) in param_registers {
            writer.write_u8(register)?;
            writer.write_string(SwfStr::from_utf8_str(name))?;
        }

        writer.write_u16(bytes.len() as u16)?;

        self.output.extend_from_slice(&bytes);
        Ok(())
    }

    pub fn write_identifier(&mut self, identifier: &Identifier<'a>, get: bool) -> Result<bool> {
        match self
            .registers
            .as_ref()
            .and_then(|registers| registers.borrow().find(identifier.base()))
        {
            Some(id) => self.stack.push(Value::Register(id)),
            None => {
                let base = self.resolve_string(identifier.base());
                self.stack.push(base);
                if get || identifier.path().is_some() {
                    self.emit(Action::GetVariable)?;
                }
            }
        }
        if let Some((last, path)) = identifier.path().and_then(|p| p.split_last()) {
            for item in path {
                self.push_cow_str(item.clone())?;
                self.emit(Action::GetMember)?;
            }
            self.push_cow_str(last.clone())?;
            if get {
                self.emit(Action::GetMember)?;
            }
            return Ok(true);
        }
        Ok(false)
    }

    pub fn write_declare(&mut self, declare: &Declare<'a>) -> Result<()> {
        let name = declare.name();
        match self
            .registers
            .as_mut()
            .and_then(|registers| registers.borrow_mut().register(name))
        {
            Some(id) => {
                if let Some(exp) = declare.value() {
                    self.write_expression(exp, true)?;
                    self.emit(Action::StoreRegister(id))?;
                    self.emit(Action::Pop)?;
                }
            }
            None => {
                let id = self.resolve_string(name);
                self.stack.push(id);
                if let Some(exp) = declare.value() {
                    self.write_expression(exp, true)?;
                    self.emit(Action::DefineLocal)?;
                } else {
                    self.emit(Action::DefineLocal2)?;
                };
            }
        }

        Ok(())
    }

    pub fn write_if(&mut self, statement: &If<'a>) -> Result<()> {
        let mut condition_buffer = Vec::new();
        let mut total: i16 = 0;
        if let Some(default) = statement.default_branch() {
            let mut action_block = crate::create_block!(self);
            action_block.write_block(default)?;
            let (bytes, other) = action_block.into_parts();
            total += bytes.len() as i16;
            condition_buffer.push((bytes, other));
        }
        for (condition, code) in statement.conditions().iter().rev() {
            let mut action_block = crate::create_block!(self);
            action_block.write_block(code)?;
            if total != 0 {
                // If total is not zero, that means there is more bytecode for this
                // if statement under this, so we need to jump over it.
                action_block.emit(Action::Jump { offset: total })?;
            }
            let (action_bytes, action_addresses) = action_block.into_parts();
            let mut condition_block = crate::create_block!(self);
            condition_block.write_expression(condition, true)?;
            condition_block.emit(Action::Not)?;
            // If the value is false, jump over the action bytecode under this.
            condition_block.emit(Action::If {
                offset: action_bytes.len() as i16,
            })?;
            condition_block.add_breakpoint_addresses(&action_addresses);
            let (mut bytes, other) = condition_block.into_parts();
            bytes.extend_from_slice(&action_bytes);
            total += bytes.len() as i16;
            condition_buffer.push((bytes, other));
        }
        for (output, break_addresses) in condition_buffer.iter().rev() {
            self.add_breakpoint_addresses(break_addresses);
            self.output.extend_from_slice(output);
        }
        Ok(())
    }

    pub fn write_while(&mut self, statement: &While<'a>) -> Result<()> {
        if statement.regular() {
            let before = self.output.len();
            self.write_expression(statement.condition(), true)?;
            self.emit(Action::Not)?;
            let total = self.output.len() - before;
            let mut block = crate::create_block!(self);
            block.write_block(statement.code())?;
            block.emit(Action::Jump {
                offset: -(total as i16) - block.len() as i16 - 10,
            })?;
            let len = block.len();
            block.resolve_addresses(
                |addr| -((addr + 7 + total) as i16),
                |addr| (len - (addr + 2)) as i16,
            )?;
            let bytes = block.into_bytes();
            self.emit(Action::If {
                offset: bytes.len() as i16,
            })?;
            self.output.extend_from_slice(&bytes);
        } else {
            let mut block = crate::create_block!(self);
            block.write_block(statement.code())?;
            let before = block.len();
            block.write_expression(statement.condition(), true)?;
            let total = block.len() - before;

            block.emit(Action::If {
                offset: -(block.len() as i16 + 5),
            })?;
            let len = block.len();
            block.resolve_addresses(
                |addr| (len - addr - 7) as i16 - total as i16,
                |addr| (len - addr - 2) as i16,
            )?;

            let bytes = block.into_bytes();
            self.output.extend_from_slice(&bytes);
        }

        Ok(())
    }

    pub fn write_for(&mut self, statement: &For<'a>) -> Result<()> {
        self.write_instruction(statement.initial())?;
        let before = self.output.len();
        self.write_expression(statement.condition(), true)?;
        self.emit(Action::Not)?;
        let total = self.output.len() - before;
        let mut block = crate::create_block!(self);
        block.write_block(statement.code())?;
        block.write_expression(statement.update(), false)?;
        block.emit(Action::Jump {
            offset: -(total as i16) - block.len() as i16 - 10,
        })?;
        let len = block.len();
        block.resolve_addresses(
            |addr| -((addr + 7 + total) as i16),
            |addr| (len - (addr + 2)) as i16,
        )?;
        let bytes = block.into_bytes();
        self.emit(Action::If {
            offset: bytes.len() as i16,
        })?;
        self.output.extend_from_slice(&bytes);
        Ok(())
    }

    pub fn write_switch(&mut self, statement: &Switch<'a>) -> Result<()> {
        self.write_expression(statement.test(), true)?;
        let mut defualt_block = crate::create_block!(self);
        if let Some(defualt) = statement.default() {
            defualt_block.write_block(defualt)?;
        }
        let defualt_bytes = defualt_block.into_bytes();
        self.emit(Action::StoreRegister(0))?;
        let mut code_block = crate::create_block!(self);
        let mut code_sizes = Vec::new();
        for (_, code) in statement.cases() {
            code_sizes.push(code_block.len() as i16);
            code_block.write_block(code)?;
        }
        let len = code_block.len();
        code_block.resolve_addresses(
            |_| 0,
            |addr| (len - addr - 2) as i16 + defualt_bytes.len() as i16,
        )?;
        let code_buffer = code_block.into_bytes();
        let mut condition_buffer = Vec::new();
        let mut total: i16 = 0;
        for (depth, ((condition, _), code_size)) in statement
            .cases()
            .iter()
            .zip(code_sizes.iter())
            .enumerate()
            .rev()
        {
            let mut block = crate::create_block!(self);
            if depth != 0 {
                // If depth is 0, that means the value should already be on the stack.
                block.stack.push(Value::Register(0));
            }
            block.write_expression(condition, true)?;
            block.emit(Action::StrictEquals)?;
            block.emit(Action::If {
                offset: total + code_size + 5,
            })?;
            total += block.len() as i16;
            condition_buffer.push(block.into_bytes());
        }
        for code in condition_buffer.iter().rev() {
            self.output.extend_from_slice(code);
        }
        self.emit(Action::Jump {
            offset: code_buffer.len() as i16,
        })?;
        self.output.extend_from_slice(&code_buffer);
        self.output.extend_from_slice(&defualt_bytes);
        Ok(())
    }

    pub fn write_with(&mut self, statement: &With<'a>) -> Result<()> {
        let mut block = crate::create_block!(self);
        block.write_block(statement.code())?;
        let with_bytes = block.into_bytes();
        self.write_expression(statement.this(), true)?;
        // We need to write the with statement manually to workaround lifetime issues.
        self.flush()?;
        let mut writer = Writer::new(&mut self.output, SWF_VERSION);
        writer.write_opcode_and_length(0x94, 2)?;
        writer.write_u16(with_bytes.len() as u16)?;
        self.output.extend_from_slice(&with_bytes);
        Ok(())
    }

    pub fn write_for_in(&mut self, statement: &ForIn<'a>) -> Result<()> {
        self.write_expression(statement.object(), true)?;
        self.emit(Action::Enumerate2)?;
        self.emit(Action::StoreRegister(0))?;
        self.stack.push(Value::Null);
        let mut block = crate::create_block!(self);
        let name = statement.iterator().name();
        if statement.declare() {
            match block
                .registers
                .as_mut()
                .and_then(|registers| registers.borrow_mut().register(name))
            {
                Some(id) => {
                    block.stack.push(Value::Register(0));
                    block.emit(Action::StoreRegister(id))?;
                    block.emit(Action::Pop)?;
                }
                None => {
                    let id = block.resolve_string(name);
                    block.stack.push(id);
                    block.stack.push(Value::Register(0));
                    block.emit(Action::DefineLocal)?;
                }
            }
        } else {
            match block
                .registers
                .as_mut()
                .and_then(|registers| registers.borrow().find(name))
            {
                Some(id) => {
                    block.stack.push(Value::Register(0));
                    block.emit(Action::StoreRegister(id))?;
                    block.emit(Action::Pop)?;
                }
                None => {
                    let id = block.resolve_string(name);
                    block.stack.push(id);
                    block.stack.push(Value::Register(0));
                    block.emit(Action::SetVariable)?;
                }
            }
        }
        block.write_block(statement.code())?;
        block.emit(Action::Jump {
            offset: -((block.len() + 19) as i16),
        })?;
        let len = block.len();
        block.resolve_addresses(
            |addr| -((addr + 16) as i16),
            |addr| (len - (addr + 2)) as i16,
        )?;
        let bytes = block.into_bytes();
        self.emit(Action::Equals2)?;
        self.emit(Action::If {
            offset: bytes.len() as i16,
        })?;
        self.output.extend_from_slice(&bytes);
        Ok(())
    }

    pub fn write_try(&mut self, statement: &Try<'a>) -> Result<()> {
        self.flush()?;
        let mut catch_buffer = Vec::new();
        let mut catch_name: Option<&'a str> = None;
        let mut last = crate::create_block!(self);
        last.emit(Action::Pop)?;
        last.stack.push(Value::Register(0));
        last.emit(Action::Throw)?;
        let last_bytes = last.into_bytes();
        let mut total = last_bytes.len() as i16;
        catch_buffer.push(last_bytes);
        for (depth, (id, code)) in statement.catches().iter().enumerate().rev() {
            let block = if let Some(sig) = id.signature() {
                let mut catch_block = crate::create_block!(self);
                let resolved = catch_block.resolve_string(id.name());
                catch_block.stack.push(resolved);
                catch_block.emit(Action::StackSwap)?;
                catch_block.emit(Action::DefineLocal)?;
                catch_block.write_block(code)?;
                if total != 0 {
                    catch_block.emit(Action::Jump { offset: total })?;
                }
                let catch_bytes = catch_block.into_bytes();
                let mut block = crate::create_block!(self);
                if depth != 0 {
                    block.emit(Action::Pop)?;
                }
                block.write_identifier(sig, true)?;
                block.stack.push(Value::Register(0));
                block.emit(Action::CastOp)?;
                block.emit(Action::PushDuplicate)?;
                block.stack.push(Value::Null);
                block.emit(Action::Equals2)?;
                block.emit(Action::If {
                    offset: catch_bytes.len() as i16,
                })?;
                block.output.extend_from_slice(&catch_bytes);
                block
            } else {
                let mut block = crate::create_block!(self);
                block.write_block(code)?;
                catch_buffer.clear();
                let bytes = block.into_bytes();
                total = bytes.len() as i16;
                catch_buffer.push(bytes);
                catch_name = Some(id.name());
                break;
            };

            total += block.len() as i16;
            catch_buffer.push(block.into_bytes());
        }
        let mut finally_block = crate::create_block!(self);
        if let Some(finally) = statement.finally() {
            finally_block.write_block(finally)?;
        }
        let finally_bytes = finally_block.into_bytes();
        let mut writer = Writer::new(&mut self.output, SWF_VERSION);
        writer.write_opcode_and_length(0x8F, 7 + catch_name.map(|n| n.len() + 1).unwrap_or(1))?;
        let mut flags = TryFlags::empty();
        flags.set(TryFlags::CATCH_BLOCK, !statement.catches().is_empty());
        flags.set(TryFlags::FINALLY_BLOCK, !finally_bytes.is_empty());
        flags.set(TryFlags::CATCH_IN_REGISTER, catch_name.is_none());
        writer.write_u8(flags.bits())?;
        let mut try_block = crate::create_block!(self);
        try_block.write_block(statement.code())?;
        if total != 0 {
            try_block.emit(Action::Jump { offset: total })?;
        }
        let try_bytes = try_block.into_bytes();
        writer.write_u16(try_bytes.len() as u16)?;
        let catch_size = catch_buffer.iter().map(|c| c.len()).sum::<usize>() as u16;
        writer.write_u16(catch_size)?;
        writer.write_u16(finally_bytes.len() as u16)?;
        if let Some(name) = catch_name {
            writer.write_string(SwfStr::from_utf8_str(name))?;
        } else {
            writer.write_u8(0)?;
        }

        self.output.extend_from_slice(&try_bytes);
        for catch in catch_buffer.iter().rev() {
            self.output.extend_from_slice(catch);
        }
        self.output.extend_from_slice(&finally_bytes);
        Ok(())
    }

    pub fn resolve_addresses(
        &mut self,
        top: impl Fn(usize) -> i16,
        bottom: impl Fn(usize) -> i16,
    ) -> Result<()> {
        for lazy in self.lazy_resolve.iter() {
            let mut writer = Writer::new(self.output.get_mut(lazy.address..).unwrap(), SWF_VERSION);
            match lazy.kind {
                ResolveType::Break => writer.write_i16(bottom(lazy.address))?,
                ResolveType::Continue => writer.write_i16(top(lazy.address))?,
            }
        }
        Ok(())
    }

    pub fn write_instruction(&mut self, instruction: &Instruction<'a>) -> Result<()> {
        match instruction {
            Instruction::Declare(declare) => self.write_declare(declare),
            Instruction::Exec(exp) => self.write_expression(exp, false),
            Instruction::If(statement) => self.write_if(statement),
            Instruction::Break => {
                self.emit(Action::Jump { offset: 0 })?;
                // NOTE: We are writing the jump, but we will not actually set
                // its value until later!
                self.lazy_resolve.push(LazyResolve {
                    kind: ResolveType::Break,
                    address: self.output.len() - 2,
                });
                Ok(())
            }
            Instruction::Continue => {
                self.emit(Action::Jump { offset: 0 })?;
                self.lazy_resolve.push(LazyResolve {
                    kind: ResolveType::Continue,
                    address: self.output.len() - 2,
                });
                Ok(())
            }
            Instruction::Throw(val) => {
                self.write_expression(val, true)?;
                self.emit(Action::Throw)
            }
            Instruction::Return(ret) => {
                if let Some(expr) = ret {
                    self.write_expression(expr, true)?;
                } else {
                    self.stack.push(Value::Undefined);
                }
                self.emit(Action::Return)
            }
            Instruction::Try(statement) => self.write_try(statement),
            Instruction::ForIn(statement) => self.write_for_in(statement),
            Instruction::Switch(statement) => self.write_switch(statement),
            Instruction::With(statement) => self.write_with(statement),
            Instruction::While(statement) => self.write_while(statement),
            Instruction::For(statement) => self.write_for(statement),
            Instruction::Nop => Ok(()),
        }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.output
    }
}

impl<'a, 'b> Default for ActionBlock<'a, 'b> {
    fn default() -> Self {
        Self {
            output: Vec::new(),
            stack: Vec::new(),
            lazy_resolve: Vec::new(),
            constant_pool: None,
            registers: None,
        }
    }
}
#[macro_export]
macro_rules! create_block {
    ($this:ident) => {
        ActionBlock::new(
            $this.constant_pool.as_mut().map(|pool| &mut **pool),
            $this.registers.as_ref().map(|registers| {
                Rc::new(RefCell::new(RegisterScope::create_child(registers.clone())))
            }),
        )
    };
}

#[macro_export]
macro_rules! create_block_with_scope {
    ($this:ident, $scope: expr) => {
        ActionBlock::new(
            $this.constant_pool.as_mut().map(|pool| &mut **pool),
            Some($scope),
        )
    };
}
