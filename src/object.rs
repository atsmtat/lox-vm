use crate::chunk::Chunk;
use crate::memory::{Gc, Trace};
use crate::value::Value;
use core::cell::RefCell;
use std::borrow;
use std::cmp;
use std::fmt;
use std::hash;

// === StrObj ===
#[derive(Debug)]
pub struct StrObj(pub String);

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

impl Trace for StrObj {
    fn trace(&mut self) {}
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

impl Trace for FnObj {
    fn trace(&mut self) {}
}

// === NativeObj ===
pub type NativeFn = fn() -> Value;

#[derive(Debug)]
pub struct NativeObj {
    pub function: NativeFn,
}

impl NativeObj {
    pub fn new(function: NativeFn) -> Self {
        NativeObj { function }
    }
}

impl fmt::Display for NativeObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<native fn>")
    }
}

impl Trace for NativeObj {
    fn trace(&mut self) {}
}

// === UpvalueObj ===
#[derive(Debug)]
pub enum Capture {
    // variable is still live/open on the stack at the given stack slot.
    Open(usize),

    // variable has been hoisted up to the heap, and is owned by this capture.
    Closed(Value),
}

#[derive(Debug)]
pub struct UpvalueObj {
    pub capture: RefCell<Capture>,
}

impl UpvalueObj {
    pub fn new(capture: Capture) -> Self {
        UpvalueObj {
            capture: RefCell::new(capture),
        }
    }
}

impl fmt::Display for UpvalueObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<upvalue>")
    }
}

impl Trace for UpvalueObj {
    fn trace(&mut self) {}
}

// === ClosureObj ===
#[derive(Debug)]
pub struct ClosureObj {
    pub function: Gc<FnObj>,
    pub captured_upvals: RefCell<Vec<Gc<UpvalueObj>>>,
}

impl ClosureObj {
    pub fn new(function: Gc<FnObj>) -> Self {
        ClosureObj {
            function,
            captured_upvals: RefCell::new(Vec::new()),
        }
    }
}

impl fmt::Display for ClosureObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", &self.function))
    }
}

impl Trace for ClosureObj {
    fn trace(&mut self) {}
}
