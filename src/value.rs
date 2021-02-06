use crate::memory::Gc;
use crate::object::{ClosureObj, FnObj, NativeObj, StrObj};
use std::cmp::PartialEq;
use std::fmt;

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Boolean(bool),
    Double(f64),
    Nil,
    String(Gc<StrObj>),
    Function(Gc<FnObj>),
    Native(Gc<NativeObj>),
    Closure(Gc<ClosureObj>),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Boolean(b) => !b,
            Value::Nil => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(val) => f.write_fmt(format_args!("{}", val)),
            Value::Double(val) => f.write_fmt(format_args!("{}", val)),
            Value::Nil => f.write_str("nil"),
            Value::String(gc_str) => f.write_fmt(format_args!("{}", &gc_str)),
            Value::Function(gc_fn) => f.write_fmt(format_args!("{}", &gc_fn)),
            Value::Native(gc_nat) => f.write_fmt(format_args!("{}", &gc_nat)),
            Value::Closure(gc_clos) => f.write_fmt(format_args!("{}", &gc_clos)),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        match self {
            Value::Boolean(lv) => match rhs {
                Value::Boolean(rv) => lv == rv,
                _ => false,
            },
            Value::Double(lv) => match rhs {
                Value::Double(rv) => lv == rv,
                _ => false,
            },
            Value::Nil => match rhs {
                Value::Nil => true,
                _ => false,
            },
            Value::String(lstr) => match rhs {
                Value::String(rstr) => lstr == rstr,
                _ => false,
            },
            _ => false,
        }
    }
}
