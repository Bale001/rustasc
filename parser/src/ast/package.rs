use super::expression::Symbol;
use super::identifier::{Identifier, ReturnType, SymbolName};
use super::instruction::Block;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Script<'a> {
    definitions: HashMap<Identifier<'a>, Symbol<'a>>,
    main: Block<'a>,
}

impl<'a> Script<'a> {
    pub fn new(definitions: HashMap<Identifier<'a>, Symbol<'a>>, main: Block<'a>) -> Self {
        Self { definitions, main }
    }

    pub fn main(&self) -> &Block<'a> {
        &self.main
    }

    pub fn definitions(&self) -> &HashMap<Identifier<'a>, Symbol<'a>> {
        &self.definitions
    }
}

#[derive(Debug)]
pub enum Package<'a> {
    Class {
        dynamic: bool,
        intrinsic: bool,
        body: PackageBody<'a>,
        implements: Vec<Identifier<'a>>,
    },
    /// package_body
    Interface(PackageBody<'a>),
}

#[derive(Debug)]
pub enum MethodKind {
    Set,
    Get,
    Regular,
}

#[derive(Debug)]
pub enum Trait<'a> {
    Variable {
        class: bool,
        public: bool,
        name: SymbolName<'a>,
        value: Option<Symbol<'a>>,
    },
    Method {
        class: bool,
        public: bool,
        name: &'a str,
        parameters: Vec<SymbolName<'a>>,
        return_type: ReturnType<'a>,
        method_kind: MethodKind,
        code: Option<Block<'a>>,
    },
}

#[derive(Debug)]
pub struct PackageBody<'a> {
    extends: Option<Identifier<'a>>,
    traits: Vec<Trait<'a>>,
}

impl<'a> PackageBody<'a> {
    pub fn new(traits: Vec<Trait<'a>>) -> Self {
        Self {
            extends: None,
            traits,
        }
    }

    pub fn set_extends(&mut self, extends: Option<Identifier<'a>>) {
        self.extends = extends;
    }
}
