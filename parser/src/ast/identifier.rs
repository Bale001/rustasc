use std::borrow::Cow;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Identifier<'a> {
    base: &'a str,
    path: Option<Rc<Vec<Cow<'a, str>>>>,
}

impl<'a> Identifier<'a> {
    pub fn new(base: &'a str) -> Self {
        Self { base, path: None }
    }

    pub fn prepend(&self, base: &'a str) -> Self {
        let mut path = vec![Cow::from(self.base)];
        path.extend_from_slice(self.path().unwrap_or(&[]));
        Self {
            base,
            path: Some(Rc::new(path)),
        }
    }

    pub fn cut(&self, size: usize) -> Self {
        // shortcut
        if size == self.len() {
            return self.clone();
        } else if size == 0 {
            return Self {
                base: self.base,
                path: None,
            };
        }
        let mut path = self.path().map(|p| p.to_vec()).unwrap_or_default();
        path.truncate(size);
        Self {
            base: self.base,
            path: Some(Rc::new(path)),
        }
    }

    pub fn base(&self) -> &'a str {
        self.base
    }

    pub fn path(&self) -> Option<&[Cow<'a, str>]> {
        self.path.as_deref().map(|p| p.deref())
    }

    pub fn len(&self) -> usize {
        self.path.as_ref().map(|p| p.len()).unwrap_or(0)
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn new_with_path(base: &'a str, path: Vec<Cow<'a, str>>) -> Self {
        Self {
            base,
            path: Some(Rc::new(path)),
        }
    }

    pub fn matches(&self, compare: Identifier<'a>) -> bool {
        if self.base() != compare.base() {
            return false;
        }
        if let (Some(current), Some(compare)) = (self.path(), compare.path()) {
            for (left, right) in current.iter().zip(compare.iter()) {
                if left != right {
                    return false;
                }
            }
        }
        true
    }
}

impl<'a> fmt::Display for Identifier<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.base)?;
        if let Some(path) = &self.path {
            for item in path.iter() {
                write!(f, ".{}", item)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct SymbolName<'a> {
    name: &'a str,
    signature: Option<Identifier<'a>>,
}

impl<'a> SymbolName<'a> {
    pub fn new(name: &'a str, signature: Option<Identifier<'a>>) -> Self {
        Self { name, signature }
    }

    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn signature(&self) -> Option<&Identifier<'a>> {
        self.signature.as_ref()
    }
}

#[derive(Debug, Clone)]
pub enum ReturnType<'a> {
    Void,
    Any,
    Value(Identifier<'a>),
}
