use crate::chunk::{Chunk, Instruction, Value};
use crate::error::InterpretError;

const STACK_MAX: usize = 256;

pub struct Vm<'a> {
    chunk: &'a Chunk,
    stack: Vec<Value>,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Vm {
            chunk,
            stack: Vec::with_capacity(STACK_MAX),
        }
    }

    fn get_number(val: Value) -> Result<f64, InterpretError> {
	match val {
	    Value::Double(v) => Ok(v),
	    _ => Err(InterpretError::RuntimeError),
	}
    }

    pub fn run(&mut self) -> Result<(), InterpretError> {
        for instr in self.chunk.iter() {
            match instr {
                Instruction::OpReturn => {
                    let val = self.pop()?;
                    println!("'{:?}'", val);
                    return Ok(());
                }

                Instruction::OpNegate => {
                    let val = self.pop()?;
                    let new_val = match val {
                        Value::Double(v) => Value::Double(-v),
                    };
                    self.push(new_val);
                }

		Instruction::OpAdd => {
		    let rhs = Self::get_number(self.pop()?)?;
		    let lhs = Self::get_number(self.pop()?)?;
		    let result = Value::Double(lhs + rhs);
		    self.push(result);
		}

		Instruction::OpSubtract => {
		    let rhs = Self::get_number(self.pop()?)?;
		    let lhs = Self::get_number(self.pop()?)?;
		    let result = Value::Double(lhs - rhs);
		    self.push(result);
		}

		Instruction::OpMultiply => {
		    let rhs = Self::get_number(self.pop()?)?;
		    let lhs = Self::get_number(self.pop()?)?;
		    let result = Value::Double(lhs * rhs);
		    self.push(result);
		}

		Instruction::OpDivide => {
		    let rhs = Self::get_number(self.pop()?)?;
		    let lhs = Self::get_number(self.pop()?)?;
		    let result = Value::Double(lhs / rhs);
		    self.push(result);
		}

                Instruction::OpConstant(val_offset) => {
                    let val = self.chunk.get_constant(val_offset);
                    self.push(val);
                }

                Instruction::OpInvalid => return Err(InterpretError::RuntimeError),
            }
        }
        Ok(())
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Result<Value, InterpretError> {
        if let Some(val) = self.stack.pop() {
            Ok(val)
        } else {
            Err(InterpretError::RuntimeError)
        }
    }
}
