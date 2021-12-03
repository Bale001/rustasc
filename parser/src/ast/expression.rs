use super::identifier::{Identifier, ReturnType, SymbolName};
use super::instruction::Block;
use super::operation::Operation;
use swf::avm1::types::FunctionFlags;

#[derive(Debug, Clone, Copy)]
pub enum Literal<'a> {
    Integer(i64),
    Number(f64),
    Bool(bool),
    String(&'a str),
    Undefined,
    Null,
}

#[derive(Debug, Clone)]
pub enum Side {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub enum Symbol<'a> {
    Literal(Literal<'a>),
    Identifier(Identifier<'a>),
    Expression(Box<Expression<'a>>),
    Not(Box<Symbol<'a>>),
    BitNot(Box<Symbol<'a>>),
    Delete(Box<Symbol<'a>>),
    Assign(Box<(Symbol<'a>, Expression<'a>)>),
    Incr(Box<Symbol<'a>>, Side),
    Decr(Box<Symbol<'a>>, Side),
    Call(Box<(Symbol<'a>, Vec<Expression<'a>>)>, bool),
    Function(
        Vec<SymbolName<'a>>,
        Block<'a>,
        ReturnType<'a>,
        FunctionFlags,
    ),
    Array(Vec<Expression<'a>>),
    Object(Vec<(&'a str, Expression<'a>)>),
    Index(Box<(Symbol<'a>, Expression<'a>)>),
    Typeof(Box<Symbol<'a>>),
}

impl<'a> Symbol<'a> {
    pub fn new_literal(literal: Literal<'a>) -> Self {
        Self::Literal(literal)
    }

    pub fn new_identifier(identifier: Identifier<'a>) -> Self {
        Self::Identifier(identifier)
    }

    pub fn new_expression(expression: Expression<'a>) -> Self {
        Self::Expression(Box::new(expression))
    }

    pub fn new_not(this: Symbol<'a>) -> Self {
        Self::Not(Box::new(this))
    }

    pub fn new_bit_not(this: Symbol<'a>) -> Self {
        Self::BitNot(Box::new(this))
    }

    pub fn new_delete(this: Symbol<'a>) -> Self {
        Self::Delete(Box::new(this))
    }

    pub fn new_assign(this: Symbol<'a>, assign: Expression<'a>) -> Self {
        Self::Assign(Box::new((this, assign)))
    }

    pub fn new_incr(this: Symbol<'a>, side: Side) -> Self {
        Self::Incr(Box::new(this), side)
    }

    pub fn new_decr(this: Symbol<'a>, side: Side) -> Self {
        Self::Decr(Box::new(this), side)
    }

    pub fn new_call(this: Symbol<'a>, arguments: Vec<Expression<'a>>, new: bool) -> Self {
        Self::Call(Box::new((this, arguments)), new)
    }

    pub fn new_function(
        parameters: Vec<SymbolName<'a>>,
        code: Block<'a>,
        return_type: ReturnType<'a>,
        flags: FunctionFlags,
    ) -> Self {
        Self::Function(parameters, code, return_type, flags)
    }

    pub fn new_array(array: Vec<Expression<'a>>) -> Self {
        Self::Array(array)
    }

    pub fn new_object(object: Vec<(&'a str, Expression<'a>)>) -> Self {
        Self::Object(object)
    }

    pub fn new_index(this: Symbol<'a>, index: Expression<'a>) -> Self {
        Self::Index(Box::new((this, index)))
    }

    pub fn new_typeof(this: Symbol<'a>) -> Self {
        Self::Typeof(Box::new(this))
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Symbol::Identifier(_) | Symbol::Index(_))
    }
}

#[derive(Debug, Clone)]
pub enum Compare {
    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct Expression<'a> {
    base: Option<Symbol<'a>>,
    arithmetic: Vec<(Operation, Symbol<'a>)>,
    ternary: Option<Box<(Expression<'a>, Expression<'a>)>>,
    compares: Vec<(Compare, Expression<'a>)>,
}

impl<'a> Expression<'a> {
    pub fn new(base: Option<Symbol<'a>>) -> Self {
        Self {
            base,
            arithmetic: Vec::new(),
            ternary: None,
            compares: Vec::new(),
        }
    }

    pub fn push(&mut self, op: Operation, value: Symbol<'a>) {
        self.arithmetic.push((op, value))
    }

    pub fn push_comp(&mut self, comp: (Compare, Expression<'a>)) {
        self.compares.push(comp)
    }

    pub fn pop(&mut self) -> Option<(Operation, Symbol<'a>)> {
        self.arithmetic.pop()
    }

    pub fn set_ternary(&mut self, left: Expression<'a>, right: Expression<'a>) {
        self.ternary = Some(Box::new((left, right)));
    }

    pub fn ternary(&self) -> Option<&(Expression<'a>, Expression<'a>)> {
        self.ternary.as_deref()
    }

    pub fn base(&self) -> Option<&Symbol<'a>> {
        self.base.as_ref()
    }

    pub fn arithmetic(&self) -> &[(Operation, Symbol<'a>)] {
        &self.arithmetic
    }

    pub fn compares(&self) -> &[(Compare, Expression<'a>)] {
        &self.compares
    }
}

impl<'a> Default for Expression<'a> {
    fn default() -> Self {
        Self {
            base: None,
            arithmetic: Vec::new(),
            ternary: None,
            compares: Vec::new(),
        }
    }
}
