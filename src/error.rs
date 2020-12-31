use crate::value::Value;
use std::error;
use std::fmt;

#[derive(Debug)]
pub struct RuntimeError {
    line: u32,
    kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    InvalidOperand(Value),
    UndefinedVariable(Value),
    InternalError(VmError),
}

#[derive(Debug)]
pub enum VmError {
    EmptyStackPop,
    InvalidOpCode,
    UnexpectedValue(Value),
}

impl RuntimeError {
    pub fn new(line: u32, kind: ErrorKind) -> Self {
        RuntimeError { line, kind }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("[line {}] Error: ", self.line))?;
        match self.kind {
            ErrorKind::InvalidOperand(val) => {
                f.write_fmt(format_args!("invalid operand {}", val))?;
            }
            ErrorKind::UndefinedVariable(val) => {
                f.write_fmt(format_args!("undefined variable {}", val))?;
            }
            ErrorKind::InternalError(ref vm_err) => match vm_err {
                VmError::EmptyStackPop => {
                    f.write_str("illegal pop on empty stack")?;
                }
                VmError::InvalidOpCode => {
                    f.write_str("invalid op code")?;
                }
                VmError::UnexpectedValue(val) => {
                    f.write_fmt(format_args!("unexpected value {}", val))?;
                }
            },
        }
        Ok(())
    }
}

impl error::Error for RuntimeError {}
