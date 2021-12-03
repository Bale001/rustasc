use crate::ast::identifier::Identifier;
use crate::linker::LinkerRoot;
use encoding_rs::UTF_8;
use std::borrow::Cow;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs;
use std::path::PathBuf;

#[derive(Debug)]
pub struct FsLinker {
    root: PathBuf,
    bases: HashSet<String>,
}

impl FsLinker {
    pub fn new(path: PathBuf) -> Self {
        let mut bases = HashSet::new();
        for entry in path.read_dir().expect("Fatal linker error") {
            let entry = entry.expect("Fatal linker error");
            let ftype = entry.file_type().expect("Fatal linker error");
            if ftype.is_dir() {
                bases.insert(entry.file_name().into_string().expect("Invalid UTF8"));
            } else if ftype.is_file() && path.extension() == Some(OsStr::new("as")) {
                let name = path.file_stem().expect("Fatal linker error").to_os_string();
                bases.insert(name.into_string().expect("Invalid UTF8"));
            }
        }
        Self { root: path, bases }
    }

    fn resolve_path<'a>(
        &self,
        mut path: PathBuf,
        next: &[Cow<'a, str>],
        total: usize,
    ) -> Option<(Vec<u8>, usize)> {
        if let Some((current, next)) = next.split_first() {
            if path.is_dir() {
                let path = path.join(&**current);
                if let Some(r) = self.resolve_path(path, next, total) {
                    return Some(r);
                }
            }
        }
        path.set_extension("as");
        match fs::read(path) {
            Ok(c) => Some((c, total - next.len())),
            Err(_) => None,
        }
    }
}

impl LinkerRoot for FsLinker {
    /// `get_listing` is implemented by mapping the base to the filesystem and reading all
    /// items in the directory
    fn get_listing<'a>(
        &self,
        base: &'a str,
        path: &[Cow<'a, str>],
    ) -> Option<Box<dyn Iterator<Item = Cow<'a, str>>>> {
        if self.bases.contains(base) {
            let mut root = self.root.join(base);
            for item in path {
                root.push(&**item);
            }
            let iter = root.read_dir().ok()?;
            let new_iter =
                iter.filter_map(|d| d.ok()?.file_name().into_string().ok().map(Cow::from));
            return Some(Box::new(new_iter));
        }
        None
    }

    fn resolve<'a>(
        &self,
        base: &'a str,
        path: &[Cow<'a, str>],
    ) -> Option<(Identifier<'a>, &'static str)> {
        if self.bases.contains(base) {
            let root = self.root.join(base);
            if let Some((content, size)) = self.resolve_path(root, path, path.len()) {
                let id = if size != 0 {
                    Identifier::new_with_path(base, path[..size].to_vec())
                } else {
                    Identifier::new(base)
                };
                let (content, _, _) = UTF_8.decode(&content);
                // TODO: Setup the lifetime of the loaded content properly.
                // This doesn't actually leak memory because the file content is never dropped anyways.
                let content = Box::leak(content.into_owned().into_boxed_str());
                return Some((id, content));
            }
        }
        None
    }
}
