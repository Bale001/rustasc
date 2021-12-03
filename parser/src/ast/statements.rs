use super::expression::Expression;
use super::identifier::SymbolName;
use super::instruction::{Block, Instruction};

#[derive(Debug, Clone)]
pub struct Declare<'a> {
    name: SymbolName<'a>,
    value: Option<Expression<'a>>,
}

impl<'a> Declare<'a> {
    pub fn new(name: SymbolName<'a>, value: Option<Expression<'a>>) -> Self {
        Self { name, value }
    }

    pub fn name(&self) -> &'a str {
        self.name.name()
    }

    pub fn value(&self) -> Option<&Expression<'a>> {
        self.value.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct If<'a> {
    conditions: Vec<(Expression<'a>, Block<'a>)>,
    default: Option<Block<'a>>,
}

impl<'a> If<'a> {
    pub fn new() -> Self {
        Self {
            conditions: Vec::new(),
            default: None,
        }
    }

    pub fn add_condition(&mut self, condition: (Expression<'a>, Block<'a>)) {
        self.conditions.push(condition);
    }

    pub fn set_default(&mut self, default: Block<'a>) {
        self.default = Some(default);
    }

    pub fn conditions(&self) -> &[(Expression<'a>, Block<'a>)] {
        &self.conditions
    }

    pub fn default_branch(&self) -> Option<&Block<'a>> {
        self.default.as_ref()
    }
}

impl<'a> Default for If<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Switch<'a> {
    test: Expression<'a>,
    cases: Vec<(Expression<'a>, Block<'a>)>,
    default: Option<Block<'a>>,
}

impl<'a> Switch<'a> {
    pub fn new(test: Expression<'a>) -> Self {
        Self {
            test,
            cases: Vec::new(),
            default: None,
        }
    }

    pub fn add_case(&mut self, case: (Expression<'a>, Block<'a>)) {
        self.cases.push(case);
    }

    pub fn set_default(&mut self, default: Block<'a>) {
        self.default = Some(default);
    }

    pub fn test(&self) -> &Expression<'a> {
        &self.test
    }

    pub fn cases(&self) -> &[(Expression<'a>, Block<'a>)] {
        &self.cases
    }

    pub fn default(&self) -> Option<&Block<'a>> {
        self.default.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct Try<'a> {
    code: Block<'a>,
    catches: Vec<(SymbolName<'a>, Block<'a>)>,
    finally: Option<Block<'a>>,
}

impl<'a> Try<'a> {
    pub fn new(code: Block<'a>) -> Self {
        Self {
            code,
            catches: Vec::new(),
            finally: None,
        }
    }

    pub fn add_catch(&mut self, catch: (SymbolName<'a>, Block<'a>)) {
        self.catches.push(catch);
    }

    pub fn set_finally(&mut self, finally: Block<'a>) {
        self.finally = Some(finally);
    }

    pub fn code(&self) -> &Block<'a> {
        &self.code
    }

    pub fn catches(&self) -> &[(SymbolName<'a>, Block<'a>)] {
        &self.catches
    }

    pub fn finally(&self) -> Option<&Block<'a>> {
        self.finally.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct With<'a> {
    this: Expression<'a>,
    code: Block<'a>,
}

impl<'a> With<'a> {
    pub fn new(this: Expression<'a>, code: Block<'a>) -> Self {
        Self { this, code }
    }

    pub fn this(&self) -> &Expression<'a> {
        &self.this
    }

    pub fn code(&self) -> &Block<'a> {
        &self.code
    }
}

#[derive(Debug, Clone)]
pub struct While<'a> {
    condition: Expression<'a>,
    code: Block<'a>,
    /// A value of false indicates this is a do-while loop.
    regular: bool,
}

impl<'a> While<'a> {
    pub fn new(condition: Expression<'a>, code: Block<'a>) -> Self {
        Self {
            condition,
            code,
            regular: true,
        }
    }

    pub fn new_do(code: Block<'a>, condition: Expression<'a>) -> Self {
        Self {
            condition,
            code,
            regular: false,
        }
    }

    pub fn regular(&self) -> bool {
        self.regular
    }

    pub fn condition(&self) -> &Expression<'a> {
        &self.condition
    }

    pub fn code(&self) -> &Block<'a> {
        &self.code
    }
}

/// for (initial; condition; update) {code}
#[derive(Debug, Clone)]
pub struct For<'a> {
    initial: Box<Instruction<'a>>,
    condition: Expression<'a>,
    update: Expression<'a>,
    code: Block<'a>,
}

impl<'a> For<'a> {
    pub fn new(
        initial: Instruction<'a>,
        condition: Expression<'a>,
        update: Expression<'a>,
        code: Block<'a>,
    ) -> Self {
        Self {
            initial: Box::new(initial),
            condition,
            update,
            code,
        }
    }

    pub fn initial(&self) -> &Instruction<'a> {
        &self.initial
    }

    pub fn condition(&self) -> &Expression<'a> {
        &self.condition
    }

    pub fn update(&self) -> &Expression<'a> {
        &self.update
    }

    pub fn code(&self) -> &Block<'a> {
        &self.code
    }
}

#[derive(Debug, Clone)]
pub struct ForIn<'a> {
    iterator: SymbolName<'a>,
    declare: bool,
    object: Expression<'a>,
    code: Block<'a>,
}

impl<'a> ForIn<'a> {
    pub fn new(
        iterator: SymbolName<'a>,
        declare: bool,
        object: Expression<'a>,
        code: Block<'a>,
    ) -> Self {
        Self {
            iterator,
            declare,
            object,
            code,
        }
    }

    pub fn iterator(&self) -> &SymbolName<'a> {
        &self.iterator
    }

    pub fn declare(&self) -> bool {
        self.declare
    }

    pub fn object(&self) -> &Expression<'a> {
        &self.object
    }

    pub fn code(&self) -> &Block<'a> {
        &self.code
    }
}
