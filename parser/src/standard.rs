use crate::ast::identifier::Identifier;
use crate::linker::LinkerRoot;
use include_dir::{include_dir, Dir, DirEntry};
use std::borrow::Cow;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::path::PathBuf;

static STANDARD_LIB: &Dir<'static> = &include_dir!("$CARGO_MANIFEST_DIR/std");
static STANDARD_LIB_8: &Dir<'static> = &include_dir!("$CARGO_MANIFEST_DIR/std8");

/// Static linker will link files stored in static memory using the include_dir! macro.
#[derive(Debug)]
pub struct StaticLinker {
    root: &'static Dir<'static>,
    bases: HashSet<&'static str>,
}

impl StaticLinker {
    pub fn new(dir: &'static Dir<'static>) -> Self {
        let mut bases = HashSet::new();
        for entry in dir.entries() {
            bases.insert(entry.path().file_stem().unwrap().to_str().unwrap());
        }
        Self { root: dir, bases }
    }

    pub fn new_standard_lib() -> Self {
        Self::new(STANDARD_LIB)
    }

    pub fn new_standard_lib_8() -> Self {
        Self::new(STANDARD_LIB_8)
    }

    fn resolve_path<'a>(
        &self,
        mut path: PathBuf,
        next: &[Cow<'a, str>],
        total: usize,
    ) -> Option<(&'static str, usize)> {
        if let Some((current, next)) = next.split_first() {
            if self.root.get_dir(&path).is_some() {
                let path = path.join(&**current);
                if let Some(r) = self.resolve_path(path, next, total) {
                    return Some(r);
                }
            }
        }
        path.set_extension("as");
        self.root
            .get_file(path)
            .map(|content| (content.contents_utf8().unwrap(), total - next.len()))
    }
}

impl LinkerRoot for StaticLinker {
    fn get_listing<'a>(
        &self,
        base: &'a str,
        path: &[Cow<'a, str>],
    ) -> Option<Box<dyn Iterator<Item = Cow<'a, str>>>> {
        let mut dir = self.root.get_dir(base)?;
        for item in path {
            dir = self.root.get_dir(&**item)?;
        }
        let iter = dir.entries().iter();
        let new_iter = iter.filter_map(|entry| match entry {
            DirEntry::Dir(d) => d.path().file_name().map(|f| Cow::from(f.to_str().unwrap())),
            DirEntry::File(f) => {
                let path = f.path();
                if path.extension() == Some(OsStr::new("as")) {
                    return path.file_stem().map(|f| Cow::from(f.to_str().unwrap()));
                }
                None
            }
        });
        Some(Box::new(new_iter))
    }

    fn resolve<'a>(
        &self,
        base: &'a str,
        path: &[Cow<'a, str>],
    ) -> Option<(Identifier<'a>, &'static str)> {
        if self.bases.contains(base) {
            if let Some((content, size)) = self.resolve_path(PathBuf::from(base), path, path.len())
            {
                let id = if size != 0 {
                    Identifier::new_with_path(base, path[..size].to_vec())
                } else {
                    Identifier::new(base)
                };
                return Some((id, content));
            }
        }
        None
    }
}
