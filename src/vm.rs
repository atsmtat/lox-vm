use crate::memory::Heap;
use crate::value::Value;
use crate::chunk::{Chunk, Instruction};
use crate::error::InterpretError;

const STACK_MAX: usize = 256;

pub struct Vm<'a> {
    chunk: &'a Chunk,
    stack: Vec<Value>,
    ip: usize,
    heap: Heap,
}

impl<'a> Vm<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
        Vm {
            chunk,
            stack: Vec::with_capacity(STACK_MAX),
	    ip: 0,
	    heap: Heap::new(),
        }
    }

    fn get_number(&mut self) -> Result<f64, InterpretError> {
	let val = self.pop()?;
	match val {
	    Value::Double(v) => Ok(v),
	    _ => {
		self.report_error( "operand must be a number" );
		return Err(InterpretError::RuntimeError);
	    }
	}
    }

    fn report_error(&self, msg: &str) {
	eprintln!( "[line {}] Error: {}", self.chunk.get_line(self.ip), msg);
    }

    pub fn run(&mut self) -> Result<(), InterpretError> {
        for (instr_index, instr) in self.chunk.iter().enumerate() {
	    self.ip = instr_index;
            match instr {
                Instruction::OpReturn => {
                    let val = self.pop()?;
                    println!("'{:?}'", val);
                    return Ok(());
                }

                Instruction::OpNegate => {
                    let val = self.get_number()?;
                    let result = Value::Double(-val);
                    self.push(result);
                }

                Instruction::OpNot => {
                    let val = self.pop()?;
                    let result = Value::Boolean(val.is_falsey());
                    self.push(result);
                }

		Instruction::OpAdd => {
		    let rhs = self.pop()?;
		    let lhs = self.pop()?;
		    let result = lhs + rhs;
		    self.push(result?);
		}

		Instruction::OpSubtract => {
		    let rhs = self.pop()?;
		    let lhs = self.pop()?;
		    let result = lhs - rhs;
		    self.push(result?);
		}

		Instruction::OpMultiply => {
		    let rhs = self.pop()?;
		    let lhs = self.pop()?;
		    let result = lhs * rhs;
		    self.push(result?);
		}

		Instruction::OpDivide => {
		    let rhs = self.pop()?;
		    let lhs = self.pop()?;
		    let result = lhs / rhs;
		    self.push(result?);
		}

		Instruction::OpEqual => {
		    let rhs = self.pop()?;
		    let lhs = self.pop()?;
		    let result = Value::Boolean(lhs == rhs);
		    self.push(result);
		}

		Instruction::OpGreater => {
		    let rhs = self.pop()?;
		    let lhs = self.pop()?;
		    let result = lhs.gt(&rhs);
		    self.push(result?);
		}

		Instruction::OpLess => {
		    let rhs = self.pop()?;
		    let lhs = self.pop()?;
		    let result = lhs.lt(&rhs);
		    self.push(result?);
		}

		Instruction::OpTrue => self.push(Value::Boolean(true)),
		Instruction::OpFalse => self.push(Value::Boolean(false)),
		Instruction::OpNil => self.push(Value::Nil),

                Instruction::OpConstant(val_offset) => {
                    let val = self.chunk.get_constant(val_offset);
                    self.push(val);
                }

                Instruction::OpInvalid => return Err(InterpretError::InternalError),
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
            Err(InterpretError::InternalError)
        }
    }
}
