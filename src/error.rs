use crate::memory::Gc;
use crate::object::StrObj;
use crate::value::Value;
use std::error;
use std::fmt;

#[derive(Debug)]
pub struct StackFrame {
    line: u32,
    fun_name: Gc<StrObj>,
}

impl StackFrame {
    pub fn new(line: u32, fun_name: Gc<StrObj>) -> Self {
        StackFrame { line, fun_name }
    }
}

impl fmt::Display for StackFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("[line {}] in {}", self.line, self.fun_name))
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    line: u32,
    kind: ErrorKind,
    stack_trace: Vec<StackFrame>,
}

#[derive(Debug)]
pub enum ErrorKind {
    InvalidOperand(Value),
    UndefinedVariable(Value),
    NonCallable(Value),
    MismatchArgCount(u8, u8),
    StackOverflow,
    InternalError(VmError),
}

#[derive(Debug)]
pub enum VmError {
    EmptyStackPop,
    InvalidOpCode,
    UnexpectedValue(Value),
}

impl RuntimeError {
    pub fn new(line: u32, kind: ErrorKind, stack_trace: Vec<StackFrame>) -> Self {
        RuntimeError {
            line,
            kind,
            stack_trace,
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Error: ")?;
        match self.kind {
            ErrorKind::InvalidOperand(val) => {
                f.write_fmt(format_args!("invalid operand {}", val))?;
            }
            ErrorKind::UndefinedVariable(val) => {
                f.write_fmt(format_args!("undefined variable {}", val))?;
            }
            ErrorKind::NonCallable(val) => {
                f.write_fmt(format_args!(
                    "{} is not callable; can only call functions and classes",
                    val
                ))?;
            }
            ErrorKind::MismatchArgCount(exp, found) => {
                f.write_fmt(format_args!(
                    "Expected {} argument(s) but got {}",
                    exp, found
                ))?;
            }
            ErrorKind::StackOverflow => {
                f.write_str("stack overflow!")?;
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

        f.write_str("\n")?;
        for frame in &self.stack_trace {
            f.write_fmt(format_args!("{}\n", frame))?;
        }

        Ok(())
    }
}

impl error::Error for RuntimeError {}
