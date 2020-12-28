use crate::error::InterpretError;
use crate::memory::{Gc, StrObj};
use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::ops::{Add, Div, Mul, Sub};

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Boolean(bool),
    Double(f64),
    Nil,
    String(Gc<StrObj>),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Boolean(b) => !b,
            Value::Nil => true,
            _ => false,
        }
    }

    pub fn gt(&self, other: &Value) -> Result<Value, InterpretError> {
        if let Some(ord) = self.partial_cmp(other) {
            match ord {
                Ordering::Greater => Ok(Value::Boolean(true)),
                _ => Ok(Value::Boolean(false)),
            }
        } else {
            Err(InterpretError::InvalidOpError)
        }
    }

    pub fn lt(&self, other: &Value) -> Result<Value, InterpretError> {
        if let Some(ord) = self.partial_cmp(other) {
            match ord {
                Ordering::Less => Ok(Value::Boolean(true)),
                _ => Ok(Value::Boolean(false)),
            }
        } else {
            Err(InterpretError::InvalidOpError)
        }
    }
}

impl Add for Value {
    type Output = Result<Value, InterpretError>;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Value::Double(lv) => match rhs {
                Value::Double(rv) => Ok(Value::Double(lv + rv)),
                _ => Err(InterpretError::InvalidOpError),
            },
            _ => Err(InterpretError::InvalidOpError),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, InterpretError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Value::Double(lv) => match rhs {
                Value::Double(rv) => Ok(Value::Double(lv - rv)),
                _ => Err(InterpretError::InvalidOpError),
            },
            _ => Err(InterpretError::InvalidOpError),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, InterpretError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Value::Double(lv) => match rhs {
                Value::Double(rv) => Ok(Value::Double(lv * rv)),
                _ => Err(InterpretError::InvalidOpError),
            },
            _ => Err(InterpretError::InvalidOpError),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, InterpretError>;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Value::Double(lv) => match rhs {
                Value::Double(rv) => Ok(Value::Double(lv / rv)),
                _ => Err(InterpretError::InvalidOpError),
            },
            _ => Err(InterpretError::InvalidOpError),
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
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match self {
            Value::Double(lv) => match rhs {
                Value::Double(rv) => lv.partial_cmp(&rv),
                _ => None,
            },
            _ => None,
        }
    }
}
