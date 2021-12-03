use super::expression::Expression;
use super::statements::{Declare, For, ForIn, If, Switch, Try, While, With};

#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    Declare(Declare<'a>),
    If(If<'a>),
    Switch(Switch<'a>),
    While(While<'a>),
    Try(Try<'a>),
    With(With<'a>),
    For(For<'a>),
    ForIn(ForIn<'a>),
    Throw(Expression<'a>),
    Return(Option<Expression<'a>>),
    Exec(Expression<'a>),
    Nop,
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Block<'a> {
    code: Vec<Instruction<'a>>,
}

impl<'a> Block<'a> {
    pub fn new(code: Vec<Instruction<'a>>) -> Self {
        Self { code }
    }

    pub fn code(&self) -> &[Instruction<'a>] {
        &self.code
    }
}

impl<'a> Default for Block<'a> {
    fn default() -> Self {
        Self { code: Vec::new() }
    }
}
