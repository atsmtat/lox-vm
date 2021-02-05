use crate::chunk::Chunk;
use crate::memory::{Gc, Trace};
use std::borrow;
use std::cmp;
use std::fmt;
use std::hash;

// === StrObj ===
#[derive(Debug)]
pub struct StrObj(pub String);

impl Trace for StrObj {}

// Since all strings are interned, string equality is same as StrObj box pointer
// equality.
impl cmp::PartialEq for Gc<StrObj> {
    fn eq(&self, other: &Gc<StrObj>) -> bool {
        self.ptr_eq(other)
    }
}

impl cmp::Eq for Gc<StrObj> {}

impl hash::Hash for Gc<StrObj> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl borrow::Borrow<String> for Gc<StrObj> {
    fn borrow(&self) -> &String {
        &self.0
    }
}

impl fmt::Display for StrObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("\"{}\"", &self.0))
    }
}

// === FnObj ===
pub struct FnObj {
    pub chunk: Chunk,
    arity: u8,
    name: Gc<StrObj>,
}

impl FnObj {
    pub fn new(chunk: Chunk, arity: u8, name: Gc<StrObj>) -> Self {
        FnObj { chunk, arity, name }
    }

    pub fn name(&self) -> Gc<StrObj> {
        self.name
    }

    pub fn arity(&self) -> u8 {
        self.arity
    }
}

impl fmt::Debug for FnObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FnObj")
            .field("arity", &self.arity)
            .field("name", &format_args!("{:?}", &self.name))
            .finish()
    }
}

impl fmt::Display for FnObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("<fn {}>", &self.name))
    }
}

impl Trace for FnObj {}
