use crate::ast::identifier::Identifier;
use crate::ast::package::{Package, Script};
use crate::parser::{ParseError, Parser};

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct SwfTree<'a> {
    main: Script<'a>,
    packages: HashMap<Identifier<'a>, PackageStage<'a>>,
}

impl<'a> SwfTree<'a> {
    pub fn new(main: &'a str, linker: Linker<'a>) -> Result<Self, ParseError> {
        let linker = Rc::new(RefCell::new(linker));
        let mut parser = Parser::new_with_linker(main, linker.clone());
        let script = parser.parse_script()?;

        let result = Ok(Self {
            main: script,
            packages: std::mem::take(&mut linker.borrow_mut().nodes),
        });
        result
    }
}

pub trait LinkerRoot: std::fmt::Debug {
    fn resolve<'a>(
        &self,
        base: &'a str,
        path: &[Cow<'a, str>],
    ) -> Option<(Identifier<'a>, &'static str)>;
    fn get_listing<'a>(
        &self,
        base: &'a str,
        path: &[Cow<'a, str>],
    ) -> Option<Box<dyn Iterator<Item = Cow<'a, str>>>>;
}

#[derive(Debug)]
pub struct Linker<'a> {
    roots: Vec<Box<dyn LinkerRoot>>,
    nodes: HashMap<Identifier<'a>, PackageStage<'a>>,
}

#[derive(Debug)]
pub enum PackageStage<'a> {
    Resolving,
    Resolved(Package<'a>),
}

pub enum Import<'a> {
    File(&'a str),
    All,
}

impl<'a> Linker<'a> {
    pub fn new(roots: Vec<Box<dyn LinkerRoot>>) -> Self {
        Self {
            roots,
            nodes: HashMap::new(),
        }
    }

    pub fn get_listing(
        &self,
        base: &'a str,
        path: &[Cow<'a, str>],
    ) -> Option<Box<dyn Iterator<Item = Cow<'a, str>>>> {
        for root in &self.roots {
            let listing = root.get_listing(base, path);
            if listing.is_some() {
                return listing;
            }
        }
        None
    }

    pub fn take_nodes(&mut self) -> HashMap<Identifier<'a>, PackageStage<'a>> {
        std::mem::take(&mut self.nodes)
    }

    pub fn find(&self, find: &Identifier<'a>) -> Option<Identifier<'a>> {
        for existing in self.nodes.keys() {
            if find != existing && existing.matches(find.clone()) {
                return Some(existing.clone());
            }
        }
        None
    }

    pub fn resolve(
        &self,
        identifier: Identifier<'a>,
    ) -> Result<Option<(Identifier<'a>, &'static str)>, ParseError> {
        if self.nodes.contains_key(&identifier) {
            return Ok(None);
        }
        for root in &self.roots {
            let resolved = root.resolve(identifier.base(), identifier.path().unwrap_or(&[]));
            if resolved.is_some() {
                return Ok(resolved);
            }
        }
        Ok(None)
    }

    pub fn set_resolving(&mut self, id: Identifier<'a>) {
        self.nodes.insert(id, PackageStage::Resolving);
    }

    pub fn set_resolved(&mut self, id: Identifier<'a>, package: Package<'a>) {
        self.nodes.insert(id, PackageStage::Resolved(package));
    }
}
